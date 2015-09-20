#lang racket/base
;;; @Package     html-parsing
;;; @Subtitle    Permissive Parsing of HTML to SXML/xexp in Racket
;;; @HomePage    http://www.neilvandyke.org/racket-html-parsing/
;;; @Author      Neil Van Dyke
;;; @Version     0.3
;;; @Date        2011-08-27
;;; @PLaneT      neil/html-parsing:1:=2

;; $Id: html-parsing.rkt,v 1.418 2011/08/28 03:49:56 neilpair Exp $

;;; @legal
;;; Copyright @copyright{} 2003--2011 Neil Van Dyke.  This program is Free
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 3 of the License (LGPL 3), or (at your option)
;;; any later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See
;;; @indicateurl{http://www.gnu.org/licenses/} for details.  For other licenses
;;; and consulting, please contact the author.
;;; @end legal

;;; @section Introduction

;;; The @code{html-parsing} library provides a permissive HTML parser.  The
;;; parser is useful for software agent extraction of information from Web
;;; pages, for programmatically transforming HTML files, and for implementing
;;; interactive Web browsers.  @code{html-parsing} emits
;;; @uref{http://www.neilvandyke.org/racket-xexp/, SXML/@i{xexp}}, so that
;;; conventional invalid HTML may be processed with XML tools such as
;;; @uref{http://pair.com/lisovsky/query/sxpath/, SXPath}.  Like Oleg
;;; Kiselyov's @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#HTML-parser,
;;; SSAX-based HTML parser}, @code{html-parsing} provides a permissive
;;; tokenizer, but @code{html-parsing} extends this by attempting to recover
;;; syntactic structure.
;;;
;;; The @code{html-parsing} parsing behavior is permissive in that it accepts
;;; erroneous HTML, handling several classes of HTML syntax errors gracefully,
;;; without yielding a parse error.  This is crucial for parsing arbitrary
;;; real-world Web pages, since many pages actually contain syntax errors that
;;; would defeat a strict or validating parser.  @code{html-parsing}'s
;;; handling of errors is intended to generally emulate popular Web browsers'
;;; interpretation of the structure of erroneous HTML.  We euphemistically term
;;; this kind of parse ``pragmatic.''
;;;
;;; @code{html-parsing} also has some support for XHTML, although XML
;;; namespace qualifiers are accepted but stripped from the resulting SXML/@i{xexp}.
;;; Note that @emph{valid} XHTML input might be better handled by a validating
;;; XML parser like Kiselyov's
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#XML-parser, SSAX}.
;;;
;;; This package obsoletes
;;; @uref{http://www.neilvandyke.org/racket-xexp/, HtmlPrag}.

(define %html-parsing:empty-token-symbol       '*empty*)
(define %html-parsing:end-token-symbol         '*end*)
(define %html-parsing:start-token-symbol       '*start*)
(define %html-parsing:entity-token-symbol      '*entity*)
(define %html-parsing:text-string-token-symbol '*text-string*)
(define %html-parsing:text-char-token-symbol   '*text-char*)

(define %html-parse:make-html-tokenizer
  ;; TODO: Have the tokenizer replace contiguous whitespace within individual
  ;; text tokens with single space characters (except for when in `pre' and
  ;; verbatim elements).  The parser will introduce new contiguous whitespace
  ;; (e.g., when text tokens are concatenated, invalid end tags are removed,
  ;; whitespace is irrelevant between certain elements), but then the parser
  ;; only has to worry about the first and last character of each string.
  ;; Perhaps the text tokens should have both leading and trailing whitespace
  ;; stripped, and contain flags for whether or not leading and trailing
  ;; whitespace occurred.
  (letrec ((no-token '())

           ;; TODO: Maybe make these three variables options.

           (verbatim-to-eof-elems '(plaintext))

           (verbatim-pair-elems '(script server style xmp))

           (ws-chars (list #\space
                           (integer->char 9)
                           (integer->char 10)
                           (integer->char 11)
                           (integer->char 12)
                           (integer->char 13)))

           (gosc/string-or-false
            (lambda (os)
              (let ((s (get-output-string os)))
                (if (string=? s "") #f s))))

           (gosc/symbol-or-false
            (lambda (os)
              (let ((s (gosc/string-or-false os)))
                (if s (string->symbol s) #f))))
           )
    (lambda (in normalized?)
      ;; TODO: Make a tokenizer option that causes XML namespace qualifiers to
      ;; be ignored.
      (letrec
          (
           ;; Port buffer with inexpensive unread of one character and slightly
           ;; more expensive pushback of second character to unread.  The
           ;; procedures themselves do no consing.  The tokenizer currently
           ;; needs two-symbol lookahead, due to ambiguous "/" while parsing
           ;; element and attribute names, which could be either empty-tag
           ;; syntax or XML qualified names.
           (c           #f)
           (next-c      #f)
           (c-consumed? #t)
           (read-c      (lambda ()
                          (if c-consumed?
                              (if next-c
                                  (begin (set! c      next-c)
                                         (set! next-c #f))
                                  (set! c (read-char in)))
                              (set! c-consumed? #t))))
           (unread-c    (lambda ()
                          (if c-consumed?
                              (set! c-consumed? #f)
                              ;; TODO: Procedure name in error message really
                              ;; isn't "%html-parse:make-html-tokenizer"...
                              (error '%html-parse:make-html-tokenizer
                                     "already unread: ~S"
                                     c))))
           (push-c      (lambda (new-c)
                          (if c-consumed?
                              (begin (set! c           new-c)
                                     (set! c-consumed? #f))
                              (if next-c
                                  (error '%html-parse:make-html-tokenizer
                                         "pushback full: ~S"
                                         c)
                                  (begin (set! next-c      c)
                                         (set! c           new-c)
                                         (set! c-consumed? #f))))))

           ;; TODO: These procedures are a temporary convenience for
           ;; enumerating the pertinent character classes, with an eye towards
           ;; removing redundant tests of character class.  These procedures
           ;; should be eliminated in a future version.
           (c-eof?      (lambda () (eof-object? c)))
           (c-amp?      (lambda () (eqv? c #\&)))
           (c-apos?     (lambda () (eqv? c #\')))
           (c-bang?     (lambda () (eqv? c #\!)))
           (c-colon?    (lambda () (eqv? c #\:)))
           (c-quot?     (lambda () (eqv? c #\")))
           (c-equals?   (lambda () (eqv? c #\=)))
           (c-gt?       (lambda () (eqv? c #\>)))
           (c-lsquare?  (lambda () (eqv? c #\[)))
           (c-lt?       (lambda () (eqv? c #\<)))
           (c-minus?    (lambda () (eqv? c #\-)))
           (c-pound?    (lambda () (eqv? c #\#)))
           (c-ques?     (lambda () (eqv? c #\?)))
           (c-semi?     (lambda () (eqv? c #\;)))
           (c-slash?    (lambda () (eqv? c #\/)))
           (c-splat?    (lambda () (eqv? c #\*)))
           (c-lf?       (lambda () (eqv? c #\newline)))
           (c-angle?    (lambda () (memv c '(#\< #\>))))
           (c-ws?       (lambda () (memv c ws-chars)))
           (c-alpha?    (lambda () (char-alphabetic? c)))
           (c-digit?    (lambda () (char-numeric? c)))
           (c-alphanum? (lambda () (or (c-alpha?) (c-digit?))))
           (c-hexlet?   (lambda () (memv c '(#\a #\b #\c #\d #\e #\f
                                             #\A #\B #\C #\D #\E #\F))))

           (skip-ws     (lambda () (read-c) (if (c-ws?) (skip-ws) (unread-c))))

           (if-read-chars
            (lambda (match-chars yes-thunk no-proc)
              (let loop ((chars       match-chars)
                         (match-count 0))
                (if (null? chars)
                    (yes-thunk)
                    (begin (read-c)
                           (if (eqv? c (car chars))
                               (begin (loop (cdr chars) (+ 1 match-count)))
                               (begin (unread-c)
                                      (no-proc match-chars match-count))))))))

           (write-chars-count
            (lambda (chars count port)
              (let loop ((chars chars)
                         (count count))
                (or (zero? count)
                    (begin (write-char (car chars) port)
                           (loop (cdr chars)
                                 (- count 1)))))))

           (make-start-token
            (if normalized?
                (lambda (name ns attrs)
                  (list name (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list name)
                      (list name (cons '@ attrs))))))

           (make-empty-token
            (lambda (name ns attrs)
              (cons %html-parsing:empty-token-symbol
                    (make-start-token name ns attrs))))

           (make-end-token
            (if normalized?
                (lambda (name ns attrs)
                  (list %html-parsing:end-token-symbol
                        name
                        (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list %html-parsing:end-token-symbol name)
                      (list %html-parsing:end-token-symbol
                            name
                            (cons '@ attrs))))))

           (make-comment-token
            (lambda (str) (list '*COMMENT* str)))

           (make-decl-token
            (lambda (parts) (cons '*DECL* parts)))

           (scan-qname
            ;; TODO: Make sure we don't accept local names that have "*", since
            ;; this can break SXML tools.  Have to validate this afterwards if
            ;; "verbatim-safe?".  Also check for "@" and maybe "@@".  Check
            ;; qname parsing code, especially for verbatim mode.  This is
            ;; important!
            (lambda (verbatim-safe?)
              ;; Note: If we accept some invalid local names, we only need two
              ;; symbols of lookahead to determine the end of a qname.
              (letrec ((os      #f)
                       (ns      '())
                       (vcolons 0)
                       (good-os (lambda ()
                                  (or os
                                      (begin (set! os (open-output-string))
                                             os)))))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((or (c-ws?) (c-splat?))
                         (if verbatim-safe?
                             (unread-c)
                             #f))
                        ((or (c-angle?) (c-equals?) (c-quot?) (c-apos?))
                         (unread-c))
                        ((c-colon?)
                         (or (null? ns)
                             (set! ns (cons ":" ns)))
                         (if os
                             (begin
                               (set! ns (cons (get-output-string os)
                                              ns))
                               (set! os #f))
                             #f)
                         (loop))
                        ((c-slash?)
                         (read-c)
                         (cond ((or (c-eof?)
                                    (c-ws?)
                                    (c-equals?)
                                    (c-apos?)
                                    (c-quot?)
                                    (c-angle?)
                                    (c-splat?))
                                (unread-c)
                                (push-c #\/))
                               (else (write-char #\/ (good-os))
                                     (write-char c   os)
                                     (loop))))
                        (else (write-char c (good-os))
                              (loop))))
                (let ((ns    (if (null? ns)
                                 #f
                                 (apply string-append
                                        (reverse ns))))
                      (localname (if os (get-output-string os) #f)))
                  (if verbatim-safe?
                      ;; TODO: Make sure we don't have ambiguous ":" or drop
                      ;; any characters!
                      (cons ns localname)
                      ;; Note: We represent "xml:" and "xmlns:" syntax as
                      ;; normal qnames, for lack of something better to do with
                      ;; them when we don't support XML namespaces.
                      ;;
                      ;; TODO: Local names are currently forced to lowercase,
                      ;; since HTML is usually case-insensitive.  If XML
                      ;; namespaces are used, we might wish to keep local names
                      ;; case-sensitive.
                      (if localname
                          (if ns
                              (if (or (string=? ns "xml")
                                      (string=? ns "xmlns"))
                                  (string->symbol (string-append ns
                                                                 ":"
                                                                 localname))
                                  (cons ns
                                        (string->symbol (string-downcase
                                                         localname))))
                              (string->symbol (string-downcase localname)))
                          (if ns
                              (string->symbol (string-downcase ns))
                              ;; TODO: Ensure in rest of code that returning #f
                              ;; as a name here is OK.
                              #f)))))))

           (scan-tag
            (lambda (start?)
              (skip-ws)
              (let ((tag-name   (scan-qname #f))
                    (tag-ns     #f)
                    (tag-attrs  #f)
                    (tag-empty? #f))
                ;; Scan element name.
                (if (pair? tag-name)
                    (begin (set! tag-ns   (car tag-name))
                           (set! tag-name (cdr tag-name)))
                    #f)
                ;; TODO: Ensure there's no case in which a #f tag-name isn't
                ;; compensated for later.
                ;;
                ;; Scan element attributes.
                (set! tag-attrs
                      (let scan-attr-list ()
                        (read-c)
                        (cond ((c-eof?)   '())
                              ((c-angle?) (unread-c) '())
                              ((c-slash?)
                               (set! tag-empty? #t)
                               (scan-attr-list))
                              ((c-alpha?)
                               (unread-c)
                               (let ((attr (scan-attr)))
                                 (cons attr (scan-attr-list))))
                              (else (scan-attr-list)))))
                ;; Find ">" or unnatural end.
                (let loop ()
                  (read-c)
                  (cond ((c-eof?)   no-token)
                        ((c-slash?) (set! tag-empty? #t) (loop))
                        ((c-gt?)    #f)
                        ((c-ws?)    (loop))
                        (else       (unread-c))))
                ;; Change the tokenizer mode if necessary.
                (cond ((not start?) #f)
                      (tag-empty?   #f)
                      ;; TODO: Maybe make one alist lookup here, instead of
                      ;; two.
                      ((memq tag-name verbatim-to-eof-elems)
                       (set! nexttok verbeof-nexttok))
                      ((memq tag-name verbatim-pair-elems)
                       (set! nexttok (make-verbpair-nexttok tag-name))))
                ;; Return a token object.
                (if start?
                    (if tag-empty?
                        (make-empty-token tag-name tag-ns tag-attrs)
                        (make-start-token tag-name tag-ns tag-attrs))
                    (make-end-token tag-name tag-ns tag-attrs)))))

           (scan-attr
            (lambda ()
              (let ((name (scan-qname #f))
                    (val  #f))
                (if (pair? name)
                    (set! name (cdr name))
                    #f)
                (let loop-equals-or-end ()
                  (read-c)
                  (cond ((c-eof?) no-token)
                        ((c-ws?)  (loop-equals-or-end))
                        ((c-equals?)
                         (let loop-quote-or-unquoted ()
                           (read-c)
                           (cond ((c-eof?) no-token)
                                 ((c-ws?) (loop-quote-or-unquoted))
                                 ((or (c-apos?) (c-quot?))
                                  (let ((term c))
                                    (set! val (open-output-string))
                                    (let loop-quoted-val ()
                                      (read-c)
                                      (cond ((c-eof?)      #f)
                                            ((eqv? c term) #f)
                                            (else (write-char c val)
                                                  (loop-quoted-val))))))
                                 ((c-angle?) (unread-c))
                                 (else
                                  (set! val (open-output-string))
                                  (write-char c val)
                                  (let loop-unquoted-val ()
                                    (read-c)
                                    (cond ((c-eof?)  no-token)
                                          ((c-apos?) #f)
                                          ((c-quot?) #f)
                                          ((or (c-ws?) (c-angle?)
                                               ;;(c-slash?)
                                               )
                                           (unread-c))
                                          ;; Note: We can treat a slash in an
                                          ;; unquoted attribute value as a
                                          ;; value constituent because the
                                          ;; slash is specially-handled only
                                          ;; for XHTML, and XHTML attribute
                                          ;; values must always be quoted.  We
                                          ;; could do lookahead for "/>", but
                                          ;; that wouldn't let us parse HTML
                                          ;; "<a href=/>" correctly, so this is
                                          ;; an easier and more correct way to
                                          ;; do things.
                                          (else (write-char c val)
                                                (loop-unquoted-val))))))))
                        (else (unread-c))))
                (if normalized?
                    (list name (if val
                                   (get-output-string val)
                                   (symbol->string name)))
                    (if val
                        (list name (get-output-string val))
                        (list name))))))

           (scan-comment
            ;; TODO: Rewrite this to use tail recursion rather than a state
            ;; variable.
            (lambda ()
              (let ((os    (open-output-string))
                    (state 'start-minus))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((c-minus?)
                         (set! state
                               (case state
                                 ((start-minus)            'start-minus-minus)
                                 ((start-minus-minus body) 'end-minus)
                                 ((end-minus)              'end-minus-minus)
                                 ((end-minus-minus) (write-char #\- os) state)
                                 (else (error '<%html-parse:make-html-tokenizer>
                                              "invalid state: ~S"
                                              state))))
                         (loop))
                        ((and (c-gt?) (eq? state 'end-minus-minus)) #f)
                        (else (case state
                                ((end-minus)       (write-char #\- os))
                                ((end-minus-minus) (display "--" os)))
                              (set! state 'body)
                              (write-char c os)
                              (loop))))
                (make-comment-token (get-output-string os)))))

           (scan-possible-cdata
            (lambda ()
              ;; Read "<!" and current character is "[", so try to read the
              ;; rest of the CDATA start delimeter.
              (if-read-chars
               '(#\C #\D #\A #\T #\A #\[)
               (lambda ()
                 ;; Successfully read CDATA section start delimiter, so read
                 ;; the section.
                 (scan-cdata))
               (lambda (chars count)
                 ;; Did not read rest of CDATA section start delimiter, so
                 ;; return a string for what we did read.
                 (let ((os (open-output-string)))
                   (display "<![" os)
                   (write-chars-count chars count os)
                   (get-output-string os))))))

           (scan-cdata
            (lambda ()
              (let ((os (open-output-string)))
                (let loop ()
                  (if-read-chars
                   '(#\] #\] #\>)
                   (lambda () (get-output-string os))
                   (lambda (chars count)
                     (if (zero? count)
                         (if (eof-object? c)
                             (get-output-string os)
                             (begin (write-char c os)
                                    (read-c)
                                    (loop)))
                         (begin (write-char #\] os)
                                (if (= count 2)
                                    (push-c #\])
                                    #f)
                                (loop)))))))))

           (scan-pi
            (lambda ()
              (skip-ws)
              (let ((name (open-output-string))
                    (val  (open-output-string)))
                (let scan-name ()
                  (read-c)
                  (cond ((c-eof?)   #f)
                        ((c-ws?)    #f)
                        ((c-alpha?) (write-char c name) (scan-name))
                        (else       (unread-c))))
                ;; TODO: Do we really want to emit #f for PI name?
                (set! name (gosc/symbol-or-false name))
                (let scan-val ()
                  (read-c)
                  (cond ((c-eof?)  #f)
                        ;; ((c-amp?) (display (scan-entity) val)
                        ;;           (scan-val))
                        ((c-ques?)
                         (read-c)
                         (cond ((c-eof?) (write-char #\? val))
                               ((c-gt?)  #f)
                               (else     (write-char #\? val)
                                         (unread-c)
                                         (scan-val))))
                        (else (write-char c val) (scan-val))))
                (list '*PI*
                      name
                      (get-output-string val)))))

           (scan-decl
            ;; TODO: Find if SXML includes declaration forms, and if so, use
            ;; whatever format SXML wants.
            ;;
            ;; TODO: Rewrite to eliminate state variables.
            (letrec
                ((scan-parts
                  (lambda ()
                    (let ((part       (open-output-string))
                          (nonsymbol? #f)
                          (state      'before)
                          (last?      #f))
                      (let loop ()
                        (read-c)
                        (cond ((c-eof?) #f)
                              ((c-ws?)
                               (case state
                                 ((before) (loop))
                                 ((quoted) (write-char c part) (loop))))
                              ((and (c-gt?) (not (eq? state 'quoted)))
                               (set! last? #t))
                              ((and (c-lt?) (not (eq? state 'quoted)))
                               (unread-c))
                              ((c-quot?)
                               (case state
                                 ((before)   (set! state 'quoted) (loop))
                                 ((unquoted) (unread-c))
                                 ((quoted)   #f)))
                              (else
                               (if (eq? state 'before)
                                   (set! state 'unquoted)
                                   #f)
                               (set! nonsymbol? (or nonsymbol?
                                                    (not (c-alphanum?))))
                               (write-char c part)
                               (loop))))
                      (set! part (get-output-string part))
                      (if (string=? part "")
                          '()
                          (cons (if (or (eq? state 'quoted) nonsymbol?)
                                    part
                                    ;; TODO: Normalize case of things we make
                                    ;; into symbols here.
                                    (string->symbol part))
                                (if last?
                                    '()
                                    (scan-parts))))))))
              (lambda () (make-decl-token (scan-parts)))))

           (scan-entity
            (lambda ()
              (read-c)
              (cond ((c-eof?) "&")
                    ((c-alpha?)
                     ;; TODO: Do entity names have a maximum length?
                     (let ((name (open-output-string)))
                       (write-char c name)
                       (let loop ()
                         (read-c)
                         (cond ((c-eof?)   #f)
                               ((c-alpha?) (write-char c name) (loop))
                               ((c-semi?)  #f)
                               (else       (unread-c))))
                       (set! name (string->symbol (get-output-string name)))
                       ;; TODO: Make the entity map an option.
                       (let ((pair (assq name '((amp  . "&")
                                                (apos . "'")
                                                (gt   . ">")
                                                (lt   . "<")
                                                (quot . "\"")))))
                         (if pair
                             (cdr pair)
                             (list '& name)))))
                    ((c-pound?)
                     (let ((num  (open-output-string))
                           (hex? #f))
                       (read-c)
                       (cond ((c-eof?)            #f)
                             ((memv c '(#\x #\X)) (set! hex? #t) (read-c)))
                       (let loop ()
                         (cond ((c-eof?)  #f)
                               ((c-semi?) #f)
                               ((or (c-digit?) (and hex? (c-hexlet?)))
                                (write-char c num)
                                (read-c)
                                (loop))
                               (else (unread-c))))
                       (set! num (get-output-string num))
                       (if (string=? num "")
                           "&#;"
                           (let ((n (string->number num (if hex? 16 10))))
                             (if (<= 32 n 126)
                                 (string (integer->char n))
                                 (integer->char n))))))
                    (else (unread-c) "&"))))

           (normal-nexttok
            (lambda ()
              (read-c)
              (cond ((c-eof?) no-token)
                    ((c-lt?)
                     (let loop ()
                       (read-c)
                       (cond ((c-eof?)   "<")
                             ;; ((c-ws?)    (loop))
                             ((c-slash?) (scan-tag #f))
                             ((c-ques?)  (scan-pi))
                             ((c-alpha?) (unread-c) (scan-tag #t))
                             ((c-bang?)
                              (read-c)
                              (if (c-lsquare?)
                                  (scan-possible-cdata)
                                  (let loop ()
                                    (cond ((c-eof?)   no-token)
                                          ((c-ws?)    (read-c) (loop))
                                          ((c-minus?) (scan-comment))
                                          (else       (unread-c)
                                                      (scan-decl))))))
                             (else (unread-c) "<"))))
                    ((c-gt?) ">")
                    (else (let ((os (open-output-string)))
                            (let loop ()
                              (cond ((c-eof?)   #f)
                                    ((c-angle?) (unread-c))
                                    ((c-amp?)
                                     (let ((entity (scan-entity)))
                                       (if (string? entity)
                                           (begin (display entity os)
                                                  (read-c)
                                                  (loop))
                                           (let ((saved-nexttok nexttok))
                                             (set! nexttok
                                                   (lambda ()
                                                     (set! nexttok
                                                           saved-nexttok)
                                                     entity))))))
                                    (else (write-char c os)
                                          (or (c-lf?)
                                              (begin (read-c) (loop))))))
                            (let ((text (get-output-string os)))
                              (if (equal? text "")
                                  (nexttok)
                                  text)))))))

           (verbeof-nexttok
            (lambda ()
              (read-c)
              (if (c-eof?)
                  no-token
                  (let ((os (open-output-string)))
                    (let loop ()
                      (or (c-eof?)
                          (begin (write-char c os)
                                 (or (c-lf?)
                                     (begin (read-c) (loop))))))
                    (get-output-string os)))))

           (make-verbpair-nexttok
            (lambda (elem-name)
              (lambda ()
                (let ((os (open-output-string)))
                  ;; Accumulate up to a newline-terminated line.
                  (let loop ()
                    (read-c)
                    (cond ((c-eof?)
                           ;; Got EOF in verbatim context, so set the normal
                           ;; nextok procedure, then fall out of loop.
                           (set! nexttok normal-nexttok))
                          ((c-lt?)
                           ;; Got "<" in verbatim context, so get next
                           ;; character.
                           (read-c)
                           (cond ((c-eof?)
                                  ;; Got "<" then EOF, so set to the normal
                                  ;; nexttok procedure, add the "<" to the
                                  ;; verbatim string, and fall out of loop.
                                  (set! nexttok normal-nexttok)
                                  (write-char #\< os))
                                 ((c-slash?)
                                  ;; Got "</", so...
                                  (read-c)
                                  (cond
                                   ((c-eof?)
                                    (display "</" os))
                                   ((c-alpha?)
                                    ;; Got "</" followed by alpha, so unread
                                    ;; the alpha, scan qname, compare...
                                    (unread-c)
                                    (let* ((vqname (scan-qname #t))
                                           (ns     (car vqname))
                                           (local  (cdr vqname)))
                                      ;; Note: We ignore XML namespace
                                      ;; qualifier for purposes of comparison.
                                      ;;
                                      ;; Note: We're interning strings here for
                                      ;; comparison when in theory there could
                                      ;; be many such unique interned strings
                                      ;; in a valid HTML document, although in
                                      ;; practice this should not be a problem.
                                      (if (and local
                                               (eqv? (string->symbol
                                                      (string-downcase local))
                                                     elem-name))
                                          ;; This is the terminator tag, so
                                          ;; scan to the end of it, set the
                                          ;; nexttok, and fall out of the loop.
                                          (begin
                                            (let scan-to-end ()
                                              (read-c)
                                              (cond ((c-eof?) #f)
                                                    ((c-gt?)  #f)
                                                    ((c-lt?)  (unread-c))
                                                    ((c-alpha?)
                                                     (unread-c)
                                                     ;; Note: This is an
                                                     ;; expensive way to skip
                                                     ;; over an attribute, but
                                                     ;; in practice more
                                                     ;; verbatim end tags will
                                                     ;; not have attributes.
                                                     (scan-attr)
                                                     (scan-to-end))
                                                    (else (scan-to-end))))
                                            (set! nexttok
                                                  (lambda ()
                                                    (set! nexttok
                                                          normal-nexttok)
                                                    (make-end-token
                                                     elem-name #f '()))))
                                          ;; This isn't the terminator tag, so
                                          ;; add to the verbatim string the
                                          ;; "</" and the characters of what we
                                          ;; were scanning as a qname, and
                                          ;; recurse in the loop.
                                          (begin
                                            (display "</" os)
                                            (if ns
                                                (begin (display ns os)
                                                       (display ":" os))
                                                #f)
                                            (if local
                                                (display local os)
                                                #f)
                                            (loop)))))
                                   (else
                                    ;; Got "</" and non-alpha, so unread new
                                    ;; character, add the "</" to verbatim
                                    ;; string, then loop.
                                    (unread-c)
                                    (display "</" os)
                                    (loop))))
                                 (else
                                  ;; Got "<" and non-slash, so unread the new
                                  ;; character, write the "<" to the verbatim
                                  ;; string, then loop.
                                  (unread-c)
                                  (write-char #\< os)
                                  (loop))))
                          (else
                           ;; Got non-"<" in verbatim context, so just add it
                           ;; to the buffer, then, if it's not a linefeed, fall
                           ;; out of the loop so that the token can be
                           ;; returned.
                           (write-char c os)
                           (or (c-lf?) (loop)))))
                  ;; Return the accumulated line string, if non-null, or call
                  ;; nexttok.
                  (or (gosc/string-or-false os) (nexttok))))))

           (nexttok #f))

        (set! nexttok normal-nexttok)
        (lambda () (nexttok))))))

(define (%html-parse:tokenize-html in normalized?)
  (let ((next-tok (%html-parse:make-html-tokenizer in normalized?)))
    (let loop ((tok (next-tok)))
      (if (null? tok)
          '()
          (cons tok (loop (next-tok)))))))

(define (%html-parse:xexp-token-kind token)
  (cond ((string? token) %html-parsing:text-string-token-symbol)
        ((char?   token) %html-parsing:text-char-token-symbol)
        ((list?   token)
         (let ((s (car token)))
           (if (memq s `(*COMMENT*
                         *DECL*
                         *PI*
                         ,%html-parsing:empty-token-symbol
                         ,%html-parsing:end-token-symbol
                         ,%html-parsing:entity-token-symbol))
               s
               %html-parsing:start-token-symbol)))
        (else (error '%html-parse:xexp-token-kind
                     "unrecognized token kind: ~S"
                     token))))

;;; @section Interface

;; @defvar %html-parse:empty-elements
;;
;; List of names of HTML element types that have no content, represented as a
;; list of symbols.  This is used internally by the parser and encoder.  The
;; effect of mutating this list is undefined.

;; TODO: Document exactly which elements these are, after we make the new
;; parameterized parser constructor.

(define %html-parse:empty-elements
  (cons '& '(area base br frame hr img input isindex keygen link meta object param
                  spacer wbr)))

;; @defproc parse-html/tokenizer tokenizer normalized?
;;
;; Emits a parse tree like @code{html->xexp} and related procedures, except
;; using @var{tokenizer} as a source of tokens, rather than tokenizing from an
;; input port.  This procedure is used internally, and generally should not be
;; called directly.

(define %html-parse:parse-html/tokenizer
  ;; Note: This algorithm was originally written in 2001 (as part of the first
  ;; Scheme library the author ever wrote), and then on 2009-08-16 was revamped
  ;; to not use mutable pairs, for PLT 4 compatibility.  It could still use
  ;; some work to be more FP, but it works for now.
  (letrec ((empty-elements
            ;; TODO: Maybe make this an option.  This might also be an
            ;; acceptable way to parse old HTML that uses the `p' element as a
            ;; paragraph terminator.
            %html-parse:empty-elements)
           (parent-constraints
            ;; TODO: Maybe make this an option.
            '((area     . (map))
              (body     . (html))
              (caption  . (table))
              (colgroup . (table))
              (dd       . (dl))
              (dt       . (dl))
              (frame    . (frameset))
              (head     . (html))
              (isindex  . (head))
              (li       . (dir menu ol ul))
              (meta     . (head))
              (noframes . (frameset))
              (option   . (select))
              (p        . (body li td th))
              (param    . (applet))
              (tbody    . (table))
              (td       . (tr))
              (th       . (tr))
              (thead    . (table))
              (title    . (head))
              (tr       . (table tbody thead))))
           (token-kinds-that-always-get-added
            `(*COMMENT*
              *DECL*
              *PI*
              ,%html-parsing:entity-token-symbol
              ,%html-parsing:text-string-token-symbol
              ,%html-parsing:text-char-token-symbol))
           (start-tag-name (lambda (tag-token) (car tag-token)))
           (end-tag-name   (lambda (tag-token) (list-ref tag-token 1))))
    (lambda (tokenizer normalized?)
      (let ((begs (list (vector #f '()))))
        (letrec ((add-thing-as-child-of-current-beg
                  (lambda (tok)
                    (let ((beg (car begs)))
                      (vector-set! beg 1 (cons tok (vector-ref beg 1))))))

                 (beg->elem
                  (lambda (beg)
                    (let ((elem-name          (vector-ref beg 0))
                          (attrs-and-contents (reverse (vector-ref beg 1))))
                      (cons elem-name attrs-and-contents))))

                 (finish-current-beg-and-return-elem
                  (lambda ()
                    (let ((elem (beg->elem (car begs))))
                      (set! begs (cdr begs))
                      (or (null? begs)
                          (add-thing-as-child-of-current-beg elem))
                      elem)))

                 (finish-current-beg
                  (lambda ()
                    (finish-current-beg-and-return-elem)))

                 (finish-all-begs-and-return-top
                  (lambda ()
                    (let loop ()
                      (let ((elem (finish-current-beg-and-return-elem)))
                        (if (car elem)
                            (loop)
                            (cdr elem))))))

                 (finish-begs-up-to-and-including-name
                  (lambda (name)
                    (let loop-find-name ((find-begs begs)
                                         (depth     1))
                      (let ((beg-name (vector-ref (car find-begs) 0)))
                        (cond ((not beg-name)
                               ;; We reached the root without finding a
                               ;; matching beg, so simply discard it.
                               (void))
                              ((eqv? name beg-name)
                               ;; We found a match, so finish the begs up to
                               ;; depth.
                               (let loop-finish ((depth depth))
                                 (or (zero? depth)
                                     (begin
                                       (finish-current-beg)
                                       (loop-finish (- depth 1))))))
                              (else
                               ;; Didn't find a match yet, and there's still at
                               ;; least one more beg to look at, so recur.
                               (loop-find-name (cdr find-begs)
                                               (+ depth 1))))))))

                 (finish-begs-upto-but-not-including-names
                  (lambda (names)
                    (let loop-find-name ((find-begs begs)
                                         (depth     0))
                      (let ((beg-name (vector-ref (car find-begs) 0)))
                        (cond ((not beg-name)
                               ;; We reached the root without finding a
                               ;; matching beg, so simply discard it.
                               (void))
                              ((memq beg-name names)
                               ;; We found a match, so finish the begs up to
                               ;; depth.
                               (let loop-finish ((depth depth))
                                 (or (zero? depth)
                                     (begin
                                       (finish-current-beg)
                                       (loop-finish (- depth 1))))))
                              (else
                               ;; Didn't find a match yet, and there's still at
                               ;; least one more beg to look at, so recur.
                               (loop-find-name (cdr find-begs)
                                               (+ depth 1)))))))))

          (let loop ()
            (let ((tok (tokenizer)))
              (if (null? tok)
                  (finish-all-begs-and-return-top)
                  (let ((kind (%html-parse:xexp-token-kind tok)))
                    (cond ((memv kind token-kinds-that-always-get-added)
                           (add-thing-as-child-of-current-beg tok))
                          ((eqv? kind %html-parsing:start-token-symbol)
                           (let* ((name (start-tag-name tok))
                                  (cell (assq name parent-constraints)))
                             (and cell
                                  (finish-begs-upto-but-not-including-names
                                   (cons 'div (cdr cell))))
                             (if (memq name empty-elements)
                                 (add-thing-as-child-of-current-beg tok)
                                 (set! begs (cons (vector (car tok)
                                                          (cdr tok))
                                                  begs)))))
                          ((eqv? kind %html-parsing:empty-token-symbol)
                           ;; Empty tag token, so just add it to current
                           ;; beginning while stripping off leading `*EMPTY*'
                           ;; symbol so that the token becomes normal SXML
                           ;; element syntax.
                           (add-thing-as-child-of-current-beg (cdr tok)))
                          ((eqv? kind %html-parsing:end-token-symbol)
                           (let ((name (end-tag-name tok)))
                             (if name
                                 ;; Try to finish to a start tag matching this
                                 ;; end tag.  If none, just drop the token,
                                 ;; though we used to add it to the current
                                 ;; beginning.
                                 (finish-begs-up-to-and-including-name
                                  name)
                                 ;; We have an anonymous end tag, so match it
                                 ;; with the most recent beginning.  If no
                                 ;; beginning to match, then just drop the
                                 ;; token, though we used to add it to the
                                 ;; current beginning.
                                 (and (vector-ref (car begs) 0)
                                      (finish-current-beg)))))
                          (else (error 'parse-html/tokenizer
                                       "unknown tag kind: ~S"
                                       kind)))
                    (loop))))))))))

;; TODO: Quote of message to a user:
;;
;; >I think this behavior is due to HtmlPrag's use in "parse-html/tokenizer"
;; >of its local "parent-constraints" variable.
;; >
;; >The following line of code from the variable binding expresses the
;; >constraint that any "p" element can have as immediate parent element
;; >only "body", "td", or "th":
;; >
;; >              (p        . (body td th))
;; >
;; >I think I know a good heuristic for dealing with unfamiliar but
;; >seemingly well-formed elements, like "page" in this case, but I'm afraid
;; >I don't have time to implement it right now.  (I am job-hunting right
;; >now, and there are many other coding things I need to do first.)
;; >
;; >Would adding "page" to the above line of the HtmlPrag source code work
;; >around the current problem, or do you need a better solution right now?

;; @defproc %parse-html input normalized? top?
;;
;; This procedure is now used internally by @code{html->xexp} and its
;; variants, and should not be used directly by programs.  The interface is
;; likely to change in future versions of HtmlPrag.

(define (%html-parse:parse-html input normalized? top?)
  (let ((parse
         (lambda ()
           (%html-parse:parse-html/tokenizer
            (%html-parse:make-html-tokenizer
             (cond ((input-port? input) input)
                   ((string?     input) (open-input-string input))
                   (else (error
                          '%html-parse:parse-html
                          "invalid input type: ~S"
                          input)))
             normalized?)
            normalized?))))
    (if top?
        (cons '*TOP* (parse))
        (parse))))

;; @defproc  html->sxml-0nf input
;; @defprocx html->sxml-1nf input
;; @defprocx html->sxml-2nf input
;; @defprocx html->sxml     input

;;; @defproc html->xexp input
;;;
;;; Permissively parse HTML from @var{input}, which is either an input port or
;;; a string, and emit an SXML/@i{xexp} equivalent or approximation.  To borrow
;;; and slightly modify an example from Kiselyov's discussion of his HTML
;;; parser:
;;;
;;; @lisp
;;; (html->xexp
;;;  "<html><head><title></title><title>whatever</title></head><body>
;;; <a href=\"url\">link</a><p align=center><ul compact style=\"aa\">
;;; <p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened</i>
;;; still &lt; bold </b></body><P> But not done yet...")
;;; @result{}
;;; (*TOP* (html (head (title) (title "whatever"))
;;;              (body "\n"
;;;                    (a (@@ (href "url")) "link")
;;;                    (p (@@ (align "center"))
;;;                       (ul (@@ (compact) (style "aa")) "\n"))
;;;                    (p "BLah"
;;;                       (*COMMENT* " comment <comment> ")
;;;                       " "
;;;                       (i " italic " (b " bold " (tt " ened")))
;;;                       "\n"
;;;                       "still < bold "))
;;;              (p " But not done yet...")))
;;; @end lisp
;;;
;;; Note that, in the emitted SXML/@i{xexp}, the text token @code{"still <
;;; bold"} is @emph{not} inside the @code{b} element, which represents an
;;; unfortunate failure to emulate all the quirks-handling behavior of some
;;; popular Web browsers.

;; (: html->xexp ((U String InputPort) -> Xexp))

(define (html->xexp input)
  (%html-parse:parse-html input #f #t))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.3 --- 2011-08-27 - PLaneT @code{(1 2)}
;;; Converted test suite from Testeez to Overeasy.
;;;
;;; @item Version 0.2 --- 2011-08-27 - PLaneT @code{(1 1)}
;;; Fixed embarrassing bug due to code tidying.  Thanks to Danny Yoo for
;;; reporting.
;;;
;;; @item Version 0.1 --- 2011-08-21 - PLaneT @code{(1 0)}
;;; Part of forked development from HtmlPrag.
;;;
;;; @end table

(provide html->xexp)
