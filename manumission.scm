;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; manumission.scm - source code for manumission HTML Helpfile builder
;;;
;;; Copyright (C) 2009 Caliber Technology LLC
;;;
;;; This file is part of manumission
;;;
;;; manumission is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; manumission is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with manumission. If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A very general diagram of the chm format:
;;;
;;; --------------------
;;; | header           |
;;; --------------------
;;; | directory        |
;;; --------------------
;;; | content sections |
;;; -------------------
;;;
;;; The directory, also referred to as a listing chunk in the
;;; specs, amounts to a alphabetically-sorted list of all the
;;; internal virtual "files" found in the content sections.
;;;
;;; There are two content sections.  The first (content section 0)
;;; is uncompressed, meaning all the virtual files within it can
;;; be read directly.  The second (content section 1) is compressed
;;; using lzx compression.  For now, we're just putting all the
;;; "files" in the uncompressed section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is a command-line tool, and the goal here is to minimize
;;; configuration duties.  We load the 'in-dir' files as pages,
;;; and look for the special names "contents.hhc" and "index.hhk"
;;; to determine the contents and index pages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module manumission
  (main main))

(define *program* "manumission")
(define *version* "0.4.1")

;; These globals are place-holders for work that's pending.  Each
;; will need to either be computed or read from config info.

(define *compatibility* 1.1)       ; 1.1 or later
(define *quickref-density* 2)      ; normally 2
(define *directory-chunk-size* #x1000)  ; always?
(define *windows-language-id* 1033)
(define *HHP-language-id* 1033)
(define *hhp-default-font* "Ariel,10,0")
(define *hhp-compiled-file* "mm")  ; file base name -> lowercase, without extension
(define *hhp-timestamp* 0)
(define *file-has-Klinks?* 0)
(define *file-has-Alinks?* 0)
(define *full-text-search-on?* 0)
(define *DBCS-in-use?* 0)
;;(define *binary-index/TOC-dword* #x1234)
(define *number-information-types* 0)
(define *nav-pane-width* 150)            ; width of the navigation pane in pixels.
(define *text/site-ImageList-offset* 0)  ; offset in /#STRINGS of the ImageList param of the "text/site 
;;                                         properties" object of the sitemap contents
(define *text/site-ImageList-folder?* 0)
(define *text/site-Background* 0)
(define *text/site-Foreground* 0)
(define *text/site-Font* 0)
(define *text/site-Window-Styles* 0)
(define *text/site-ExWindow-Styles* 0)
(define *text/site-FrameName* 0)
(define *text/site-WindowName* 0)
(define *number-of-MERGE-FILES* 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define minus1 (string-hex-intern "FFFFFFFF"))
(define zero   (string-hex-intern "00000000"))

;; dump string as hex
(define (hex-dump str)
  (define (pad s len)
    (let ((slen (string-length s)))
      (if (= slen len)
          s
          (string-append (make-string (- len slen) #\0) s))))
  (let ((data (list-split (map char->integer (string->list str)) 4 0)))
    (for-each
      (lambda (g)
       (display
        (pad
         (apply
           string-append
           (map (lambda (e) (format "~a" (pad (number->string e 16) 2))) g))
          8))
       (display " "))
   data))
  str)

(define (string-trim-left str)
  (pregexp-replace* "^\\s+" str ""))

(define (string-trim-right str)
  (pregexp-replace* "\\s+$" str ""))

(define (string-trim str)
  (string-trim-left (string-trim-right str)))

;; remove any duplicate items
(define (keep-unique lst)
  (let loop ((lst lst)
	     (acc '()))
    (if (null? lst)
	(reverse acc)
	(let ((item (car lst)))
	  (loop (cdr lst) 
		(if (member item (cdr lst))
		    acc
		    (cons item acc)))))))

;; remove elements unless test is #t
(define (keep-not test lst)
  (filter
   (lambda (e)
     (not (test e)))
   lst))

;; compose 2 functions:
(define (compose f g)
  (lambda args
    (f (apply g args))))

(define-record-type topic
  (make-topic name section data attributes)
  topic?
  (name topic-name set-topic-name!)
  (section topic-section set-topic-section!)
  (data topic-data set-topic-data!)
  (attributes topic-attributes set-topic-attributes))

;; loads html files into a list of
;; '(path content-string . other)
;; currently the optional third element is used
;; to indicate special pages
(define (load-dir dir default-topic contents-topic index-topic)
  ;;(printf "load-dir: ~s ~s ~s ~s\n" dir default-topic contents-topic index-topic)
  (let ((names (directory->list dir)))
    (filter-map (lambda (p)
		  (if (or (directory? p)
			  (member p '("contents.txt" "index.txt")))
		    #f
		    ;;(printf "p: ~s\n" p)
		    (make-topic
		      (format "/~a" p)
		      0   ; section 0 for now
		      (file->string (format "~a/~a" dir p))
		      (cond
			((string-ci=? p default-topic) 'default)
			((string-ci=? p contents-topic) 'contents)
			((string-ci=? p index-topic) 'index)
			(else #f)))))
		names)))

;; a special topic, like contents or index?
(define (special? topic)
  (memq (topic-attributes topic) '(contents default index)))

;; return the filename for a special topic
(define (find-topic-name sym topics)
  (let* ((a-list (filter-map
		  (lambda (topic)
		    (if (special? topic)
			(cons (topic-attributes topic) topic)
			#f))
		  topics))
	 (fnd (assq sym a-list)))
    (if fnd (ignore-/ (topic-name (cdr fnd))) "")))
    
;; drop initial "/" in path
(define (ignore-/ path)
  (if (and (not (string=? path ""))
	   (eq? #\/ (string-ref path 0)))
      (substring path 1)
      path))

;; return the title of html page
(define (page-title str)
  (let* ((p1 "(?:T|t)(?:I|i)(?:T|t)(?:L|l)(?:E|e)")
	 (open (format "<~a>" p1))
	 (close (format "</~a>" p1))
	 (m (pregexp-match (format "~a(.*)~a" open close) str)))
    (if m
	(cadr m)
	#f)))

;; bitwise-or of multiple values
(define (bit-or* . args)
  (let loop ((lst args)
	     (acc 0))
    (if (null? lst)
	acc
	(loop (cdr lst)
	      (bit-or (car lst) acc)))))

;; filters out #f items in lst
(define (remove-false lst)
  (let loop ((lst lst)
	     (acc '()))
    (if (null? lst)
	(reverse acc)
	(loop (cdr lst)
	      (if (car lst)
		  (cons (car lst) acc)
		  acc)))))

;; converts from string to utf-8
(define (utf-8 str)
  str)

;; converts from string to utf-16.
;; (we just insert null bytes)
(define (utf-16 str)
  (let loop ((lst (string->list str))
	     (acc '()))
    (if (null? lst)
	(list->string (reverse acc))
	(loop (cdr lst)
	      (append (list #\null (car lst)) acc)))))

;; helper for integer output
(define (int->bytes val len)
  (list->string
   (map
    integer->char
    (let loop ((n val)
	       (acc '()))
      (if (>= (length acc) len)
	  (reverse acc)
	  (loop (quotient n 256)
		(cons (remainder n 256) acc)))))))

;; returns val as a QWORD byte string
(define (qword val)
  (int->bytes val 8))

;; returns val as a DWORD byte string
(define (dword val)
  (int->bytes val 4))

;; returns val as a WORD byte string
(define (word val)
  (int->bytes val 2))

;; returns the which'th byte from val
(define (get-byte val which)
  (make-string
   1
   (integer->char
    (bit-rsh
     (bit-and
      (bit-lsh #xFF (* 8 which)) val) (* 8 which)))))

;; returns a WORD with bytes reversed
(define (word-as-2-bytes val)
  (string-append
   (get-byte val 1)
   (get-byte val 0)))

;; total number of bytes in a list of byte strings
(define (byte-list-total blist)
  (string-length (apply string-append blist)))

;; returns the byte-string for an ENCINT
(define (encint val)
  (let loop ((v (bit-rsh val 7))
	     (acc (list (make-string 1 (integer->char (bit-and #x7F val))))))
    (if (zero? v)
	(apply string-append acc)
	(loop (bit-rsh v 7)
	      (cons (make-string 1 (integer->char (bit-or #x80 (bit-and #x7F v)))) acc)))))

;; returns the guid as a byte-string
(define (guid str)
  ;; 7C 01 FD 10 => 10 FD 01 7C
  (define (bytes-out s)
    (let loop ((lst (string->list s))
	       (acc '()))
      (if (null? lst)
	(string-hex-intern (list->string acc))
	(loop (cddr lst)
	      (cons (car lst) (cons (cadr lst) acc))))))
  (let* ((val (if (eq? #\{ (string-ref str 0))
		  (substring str 1 (- (string-length str) 1))))
	 (parts (string-split val "-")))
    (string-append
     (bytes-out (car parts))
     (bytes-out (list-ref parts 1))
     (bytes-out (list-ref parts 2))
     (string-hex-intern (list-ref parts 3))
     (string-hex-intern (list-ref parts 4))
     (string-hex-intern (list-ref parts 5))
     (string-hex-intern (list-ref parts 6)))))

;; is the topic in the TOC?
(define (in-contents? topic)
  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now we're getting serious
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first bytes in the chm file
(define (prolog)
  (string-append
   "ITSF"
   (dword 3)      ; version
   (dword 96)     ; header length
   (dword 1)
   (dword 0)    ; timestamp
   (dword *windows-language-id*)  ; Windows language id
   (guid "{7C01FD10-7BAA-11D0-9E0C-00A0-C922-E6EC}")
   (guid "{7C01FD11-7BAA-11D0-9E0C-00A0-C922-E6EC}")))

;; the header-section-table
(define (header-section-table prolog headers)
  (apply string-append
	 (let loop ((lst (reverse headers))
		    (acc '()))
	   (if (null? lst)
	       acc
	       (loop (cdr lst)
		     (cons
		      (string-append
		       ;; section header offset from file start
		       (qword (+ (string-length prolog)
				 ;; section table length
				 (* (length headers) #x10) 8
				 ;; length of preceding header sections:
				 (if (= 1 (length lst)) 0
				     (string-length (apply string-append (cdr lst))))))
		       ;; section header table - length
		       (qword (string-length (car lst))))
		      acc))))))

;; header section 0
(define (header-section0 filesize)
  (apply string-append
	 (list
	  (dword #x01FE)
	  zero
	  (qword filesize)
	  zero
	  zero)))

;; header section 1
(define (header-section1 index-chunks dir-listings)
  (string-append
   (apply string-append
	  (list
	   "ITSP"
	   (dword 1)       ; version
	   (dword #x54)    ; length of directory header
	   (dword #x0a)    ; unknown
	   (dword *directory-chunk-size*)
	   (dword *quickref-density*)
	   (dword (if (null? index-chunks) 1 2))
	   (if (null? index-chunks) minus1 (dword (length dir-listings)))
	   (dword (- (length index-chunks) 1))  ; first pgml
	   (dword (+ (length index-chunks)
		     (length dir-listings)
		     -2))                       ; last pgml
	   minus1
	   (dword (+ (length index-chunks) (length dir-listings)))
	   (dword *windows-language-id*)
	   (guid "{5D02926A-212E-11D0-9DF9-00A0-C922-E6EC}")
	   (dword #x54)    ; length of directory header
	   minus1
	   minus1
	   minus1))
   (apply
    string-append 
    (append
     (map car dir-listings)
     index-chunks))))

;; returns a list of quickref entries
;; 1 entry for every fifth file
;; each is an offset from entry 0
;; entries is reversed!
(define (quickref e-list)
  (let ((modulus (+ 1 (bit-lsh *quickref-density* 1))))
    (let loop ((lst (reverse e-list))
	       (count 0)
	       (offset 0)
	       (acc (list (length e-list))))    ;; last entry is number of entries
      (cond
       ((null? lst) acc)
       ((and (not (zero? count))
	     (zero? (modulo count modulus)))
	(loop (cdr lst) (+ 1 count)
	      (+ offset (string-length (car lst)))
	      (cons offset acc)))
       (else (loop (cdr lst)
		   (+ 1 count)
		   (+ offset (string-length (car lst)))
		   acc))))))

;; directory listing-chunks
;; f-list is ((name content-section-number offset length) ...)
;; which is filename, content-section-number, offset in content
;; section.
;; returns a list of
;; (chunk count first-name)
;; where chunk is the listing-chunk string, 
;; and first-name is the name of its first entry
;; (used when building an index chunk).
;; count is ignored in return.
(define (listing-chunks f-list)
  ;;(hex-dump (caddr (assoc "::DataSpace/NameList" f-list)))
  (let loop ((lst f-list)
	     (chunk-num 0)
	     (acc '()))
    (if (null? lst)
	(reverse acc)
	(let* ((chunk (make-listing-chunk chunk-num lst))
	       (remaining (drop lst (cadr chunk))))
	  (loop remaining (+ chunk-num 1) (cons chunk acc))))))

;; dir-chunks is list returned by listing-chunks above
;; here we index the listing-chunks, but only if there's more
;; than 1.
(define (indexing-chunk dir-chunks)
  (define (make-entry e dir-num)
    (let ((nm (caddr e)))
      (string-append
	(encint (string-length nm))
	(utf-8 nm)
	(encint dir-num))))
  (if (= (length dir-chunks) 1)
    '()
    (let loop ((lst dir-chunks)
	       (count 0)
	       (free-space (- *directory-chunk-size* 8))
	       (acc '()))
      (if (null? lst)
	(let* ((quicks (map word (quickref acc)))
	       (free-space (- *directory-chunk-size*
			      8   ; offset of first entry in the chunk
			      (byte-list-total acc)))
	       (padding-len (- free-space (byte-list-total quicks))))
	  (list
	    (string-append
	      (apply
		string-append
		"PMGI"
		(dword free-space)
		(reverse acc))
	      (make-string padding-len)
	      (apply string-append quicks))))
	(let ((e (make-entry (car lst) count)))
	  (loop (cdr lst)
		(+ 1 count)
		(- free-space (string-length e))
		(cons e acc)))))))

;; bytes used by current quickref area:
(define (quick-space entries)
  (* 4
     (+ 1
	(quotient
	 (length entries)
	 (+ 1 (bit-lsh #x1 *quickref-density*))))))

;; takes the most entries it can from f-list,
;; makes a listing-chunk, and returns a list of
;; (chunk count first-name)
;; where count is the number of names used
;; and first-name the first entry in the chunk
(define (make-listing-chunk chunk-num f-list)
  (define last-chunk (- (length f-list) 1))
  ;;(printf "f-list:\n") (pp f-list)
  ;; returns a single directory entry
  ;; name is a string
  (define (make-entry name section-num offset len)
    ;;(printf "make-entry ~s ~s ~s ~s\n" name section-num offset len)
    (string-append
      (encint (string-length name))
      (utf-8 name)
      (encint section-num)
      (encint offset)
      (encint len)))
  (let loop ((lst f-list)
	     (count 0)
	     (offset 0)
	     (first-name #f)
	     (entries '()))
    ;;(fprintf (current-error-port) "chunk-num: ~s count: ~s\n" chunk-num count)
    (if (null? lst)
	(let* ((quicks (map word (quickref entries)))
	       (free-space (- *directory-chunk-size*
			      #x14   ; offset of first entry in the chunk
			      (byte-list-total entries)))
	       (padding-len (- free-space (byte-list-total quicks))))
	  (list
	    (string-append
	      "PMGL"
	      (dword free-space)  ; Length of free space and/or quickref area at end of directory chunk
	      zero
	      (if (= chunk-num 0) minus1 (dword chunk-num))  ; prev chunk
	      (if (>= count (length f-list)) minus1 (dword (+ chunk-num 1)))  ; next chunk
	      (apply string-append (reverse entries))
	      (make-string padding-len)
	      (apply string-append quicks))
	    count
	    first-name))
	(let* ((item (car lst))
	       (entry (make-entry (car item) (cadr item) (cadddr item) (list-ref item 4)))
	       (name (caar lst))
	       (free-space (- *directory-chunk-size*
			      #x14   ; offset of first entry in the chunk
			      (byte-list-total entries))))
	  (cond
	   ((positive? (- free-space
			  (string-length entry)
			  (quick-space entries)))
	    (loop (cdr lst)
		  (+ count 1)
		  (+ offset (string-length entry))
		  (if first-name first-name name)
		  (cons entry entries)))
	   (else (loop '() count offset first-name entries)))))))

;; start of file
(define (header index-chunk-list dir-listings filesize)
    (let* ((p0 (prolog))                                         ; 0x38 bytes
	   (h0 (header-section0 filesize))                       ; 0x18 bytes
	   (h1 (header-section1 index-chunk-list dir-listings))  ; 0x4C bytes
	   (tbl (header-section-table p0 (list h0 h1)))          ; 0x28 bytes
	   (h-addl (qword (+ 8                                   ; 0x08 bytes
			     (string-length p0)                  ; ----------
			     (string-length tbl)                 ; 0xCC bytes
			     (string-length h0)
			     (string-length h1)))))
      (string-append p0 tbl h-addl h0 h1)))

;; returns the #SYSTEM coded entry
;; if add-NT? is #t, add a null-terminator
(define (system$entry code data . add-NT?)
  (let* ((add-nt? (if (null? add-NT?) #f (car add-NT?)))
	 (entry (if add-nt? (string-append data (make-string 1 #\000)) data)))
    (string-append
     (word code) (word (string-length entry)) entry)))

;; creates the #SYSTEM "file"
(define (system$ topics title idxhdr)
  (string-append
   (dword (if (= *compatibility* 1) 2 3)) ; version
   (system$entry 10 zero)            ; timestamp
   (system$entry  9 (format "~a v~a" *program* *version*) #t)
   (system$entry  4 (string-append (dword *HHP-language-id*)
				   (dword *DBCS-in-use?*)
				   (dword *full-text-search-on?*)
				   (dword *file-has-Klinks?*)
				   (dword *file-has-Alinks?*)
				   (qword *hhp-timestamp*)
				   zero       ; unknown
				   zero))     ; unknown
   (system$entry  2 (find-topic-name 'default topics) #t)
   (system$entry  3 title #t)
   (system$entry 16 *hhp-default-font* #t)
   (system$entry  6 *hhp-compiled-file* #t)
   (system$entry  0 (find-topic-name 'contents topics) #t)
   (system$entry  1 (find-topic-name 'index topics) #t)
   ;;(system$entry  7 (dword *binary-index/TOC-dword*))   ; when Binary Index on, must match dword at /#URLTBL->sitemap
   ;;(system$entry 11 (dword *binary-index/TOC-dword*))   ; when Binary TOC on, must match dword at /#URLTBL->sitemap
   (system$entry 12 (dword *number-information-types*))
   (system$entry 13 idxhdr)  ; doesn't seem to be necessary, but we'll keep it for now
   ;; skip code 14
   ;; skip code 8
   (if (= *compatibility* 1)
       ""
       (system$entry 15 zero))  ; Information type checksum. Unknown algorithm & data source.
   ))

;; the #IDXHDR "file"
(define (idxhdr$ topics)
  (string-append
   "T#SM"
   (dword 8653545)   ; offset  4 unknown (timestamp/checksum ?)
   (dword 1)   ; offset  8 unknown
   (dword (length topics)) ; offset  C
   zero   ; offset 10 unknown
   (dword *text/site-ImageList-offset*)    ; offset 14
   zero   ; offset 18 unknown
   (dword *text/site-ImageList-folder?*)   ; offset 1C
   (dword *text/site-Background*)          ; offset 20
   (dword *text/site-Foreground*)          ; offset 24
   (dword *text/site-Font*)                ; offset 28
   (dword *text/site-Window-Styles*)       ; offset 2C
   (dword *text/site-FrameName*)           ; offset 38
   (dword *text/site-ExWindow-Styles*)     ; offset 30
   zero ; offset 34 Unknown. Often -1. Sometimes 0.
   (dword *text/site-WindowName*)          ; offset 3C
   (dword *number-information-types*)      ; offset 40
   (dword 1) ; offset 44 Unknown.  Often 1. Also 0, 3.
   (dword *number-of-MERGE-FILES*)         ; offset 48
   zero ; offset 4C Unknown.  Often 0. Non-zero mostly in files with some files in the merge files list.
   (make-string 4016 #\000); offset 50 List of offsets in the #STRINGS file that are the [MERGE FILES] list
   ))

;; the #STRINGS "file"
;; string-list is a list of strings for the file
(define (strings$ string-list)
  (let ((null (make-string 1 #\null)))
    (apply string-append 
	   (cons null
		 (map (lambda (s)
			(string-append
			  (utf-8 s)
			  null))
		      string-list)))))

;; computes the offset for a given string within the #STRINGS file
(define (strings$offset s string-list)
  (let ((s (ignore-/ s)))  ; in case it's a filename
    (let loop ((lst string-list)
	       (offset 1))      ; the string at offset 0 is a null
      (cond
       ((null? lst) #f)
       ((or (not s) (string=? s "")) 0)
       ((string=? (car lst) s) offset)
       (else
	(loop (cdr lst) (+ offset 1 (string-length (car lst)))))))))

;; the #TOPICS "file"
(define (topics$ topics string-list)
  (apply
   string-append
   (filter-map
    (lambda (topic)
      (if (special? topic)
	  #f
	  (let* ((title (or (page-title (topic-data topic)) 
			    (ignore-/ (topic-name topic))))
		 (offset (strings$offset title string-list)))
	    (string-append
	     zero          ; Offset into the tree in the #TOCIDX file.
	     (dword offset)     ; title tag (#STRINGS offset)
	     zero          ; Offset in #URLTBL of entry containing offset to #URLSTR entry containing the URL
	     (word (if (in-contents? topic) 6 2))
	     (word 0)           ; unknown
	     ))))
    topics)))

;; the #URLSTR "file"
;; must be in 4000 byte blocks, except the last block
(define (urlstr$ topics)
  (string-append
   (apply
    string-append
    (filter-map
     (lambda (topic)
       (if (special? topic) ; a special topic like contents or index
	   #f
	   (string-append
	    (make-string 1 #\000)    ; unknown
	    zero           ; offset of URL
	    zero           ; offset of FrameName
	    (ignore-/ (topic-name topic))     ; local for this topic
	    )))
     topics))
   (make-string 1 #\000)   ; null terminator
   ))

;; the #URLTBL "file"
;; must be in broken up into 4096-byte blocks
(define (urltbl$)
  (let ((entries (string-append
		  zero ; unknown
		  zero ; Index of entry in #TOPICS file
		  (dword 1) ; Offset in #URLSTR file of entry containing filename
		  )))
    entries))
  
#;
(define (btree-header index)
  (string-append
    ";)"
    (dword #x8000104)
    "X44\000\000\000\000\000\000\000\000\000\000\000\000\000"
    zero
    zero  ; last listing block
    zero  ; root block
    minus1
    (dword 1)  ; number of blocks
    (word 1)   ; depth of index (1 = no index, 2 = one level of index, ...)
    (dword 1)  ; number of keywords
    (dword 1252) ; codepage id
    (dword *HHP-language-id*)
    (dword 1)
    (dword #x272f)
    zero
    zero
    zero))

#;
(define (btree-listing-head index)
  (string-append
    (word 1974) ; free space at end of block
    (word 1)    ; number of entries in block
    minus1 ; index of prev block
    minus1)) ; index of next block

#;
(define (btree-listing index)
  (string-append
    (btree-listing-head index)))

#;
;; the $WWKeywordLinks/BTree file
(define (btree index)
  (define (txt-read)
    (call-with-input-file index
     (lambda (p)
      (text-parse p))))
  (define (text-parse port)
    (let ((grammar
            (regular-grammar ()
              ((: "#" all) (print 'comment) (ignore))
              ((in " \t\r\n") (ignore))
              ((+ (out ", ")) (list 'ID (the-string)))
              ("," 'COMMA)))
          (lang
            (lalr-grammar (COMMA ID)
             (lines
              (() '())
              ((lines line) (cons line lines)))
             (line
              ((ID@id1 COMMA ID@id2 COMMA ID@path) (list id1 id2 path))))))
      (read/lalrp lang grammar port)))
  (let ((ndx-data (txt-read)))
   (pp ndx-data)
   (string-append
     (btree-header index)
     (btree-listing index))))

;; the NameList "file"
(define (namelist section-names)
  (let ((entry-list
	 (map (lambda (name)
		(string-append (word (string-length name))
			       (utf-16 name)
			       (word 0)))
	      section-names)))
    (string-append
     (word (/ (+ 4 (byte-list-total entry-list)) 2))  ; length of file in words
     (word (length entry-list))                 ; number of entries
     (apply string-append entry-list))))        ; the entries

;; lzx compression of an empty string:
(define (lzx-data)
  (string-append
   "\377\377\377\377\b\020\000\000\000\000\000\000\000\000"
   "\001\000\007\001\377\377\340\377\000\000\000\000\000\000"
   "\004\000\031B\276\317\357\373\371\276\000\340\000\000\000"
   "\000\000\000B\004\277\017\373\364\337\342\377\377\377\377"
   "\377\377\377\377\377\377\377\377\377\377\377\377\377\377"
   "\377\377\377\377\377\377\377\377\377\377\377\377\300\377"))

(define (html? path)
  (pregexp-match "\\.html$" path))

;; for each html file in dir, list the title and the path
;; into the file "index.txt".
;; result will look like:
;; <title 1> (path1.html)
;;    :
(define (generate-index dir)
  (let ((html-files (filter html? (directory->list dir))))
    (printf "Generating ~a/index.txt from html documents. Edit this file to control which pages appear in the \"index\" tab of the help viewer.\n" dir)
    (with-output-to-file
      (format "~a/index.txt" dir)
      (lambda ()
	(printf "# Title (path):\n")
	(for-each
	  (lambda (path)
	    (printf "~a (~a)\n" (page-title (file->string (format "~a/~a" dir path))) path))
	  html-files)))))

;; for each html file in dir, list the title and the path
;; into the file "contents.txt".
;; result will look like:
;; Reference
;;	<title 1> (path1.html)
;;       :
(define (generate-contents dir)
  (let ((html-files (filter html? (directory->list dir))))
    (printf "Generating ~a/contents.txt from html documents. Edit this file to organize your table of contents.\n" dir)
    (with-output-to-file
      (format "~a/contents.txt" dir)
      (lambda ()
	(printf "# Group topics by indenting them under a category name like 'Reference' below.
# Each topic should be of the format Title (path).\n")
	(print "Reference")
	(for-each
	  (lambda (path)
	    (printf "\t~a (~a)\n" (page-title (file->string (format "~a/~a" dir path))) path))
	  html-files)))))

;; convert the text contents file to the html format expected in the chm,
;; and write it out to "index.hhk" in the input directory
(define (convert-index path dir)
  (define (output title . path)
    (string-append
      (format "<li><object type=\"text/sitemap\"><param name=\"Name\" value=\"~a\">\n" title)
	(if (null? path)
	  ""
	(format "<param name=\"Local\" value=\"~a\">\n" (car path)))
	"</object>"))
  (define (open)
    (format "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
<head>
<meta name=\"GENERATOR\" content=\"Manumission Helpfile Generator v~a\">
</head>
<body>
<ul>" *version*))
  (define (close str)
    (format "~a</ul>\n</body>\n</html>\n" str))
  (let loop ((lines (with-input-from-file path read-lines))
	     (acc (open)))
    (cond
      ((null? lines)
       (let ((outpath (format "~a/index.hhk" dir)))
	 (with-output-to-file outpath (lambda () (display (close acc))))
	 (basename outpath)))
      ((pregexp-match "^#" (car lines)) (loop (cdr lines) acc))
      (else
	(let* ((m (pregexp-match "\\s*(.*?)\\s*\\((.*)\\)$" (car lines)))
	       (title (and m (cadr m)))
	       (path (and m (caddr m))))
	  (loop (cdr lines) (format "~a\n~a" acc (output title path))))))))

;; convert the text contents file to the html format expected in the chm,
;; and write it out to "contents.hhc" in the input directory
(define (convert-contents path dir)
  ;; we'll count each #\tab or #\space
  (define (count-tabs str)
    (let loop ((lst (string->list str))
	       (count 0))
      (cond
	((null? lst) count)
	((memq (car lst) '(#\space #\tab)) (loop (cdr lst) (+ 1 count)))
	(else count))))
  (define (page? line)
    (pregexp-match "\\s*(.*?)\\s*\\((.*)\\)$" line))
  (define (output title . path)
    (string-append
      (format "<li><object type=\"text/sitemap\"><param name=\"Name\" value=\"~a\">" title)
	(if (null? path)
	  ""
	(format "\n <param name=\"Local\" value=\"~a\">" (car path)))
	"</object></li>\n"))
  (define (open)
    (format "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
<head>
<meta name=\"GENERATOR\" content=\"Manumission Helpfile Generator v~a\">
</head>
<body>
<ul>\n" *version*))
  (define (close str)
    (format "~a</ul>\n</ul>\n</body>\n</html>\n" str))  ; here we're assuming last entry was a page!
  (let loop ((lines (with-input-from-file path read-lines))
	     (indent -1)
	     (acc (open)))
    (cond
      ((null? lines)
       (let ((outpath (format "~a/contents.hhc" dir)))
	 (with-output-to-file outpath (lambda () (display (close acc))))
	 (basename outpath)))
      ((pregexp-match "^#" (car lines)) (loop (cdr lines) indent acc))
      ((page? (car lines))
       => (lambda (m)
	    (let ((title (cadr m))
		  (path (caddr m)))
	      ;;(fprintf (current-error-port) "title ~s path: ~s\n" title path)
	      (loop (cdr lines) indent (format "~a~a" acc (output title path))))))
      (else  ;; this line must be a heading, so the indentation is changing
	(let* ((tabs (count-tabs (car lines)))
	       (close-elem (if (> tabs indent)
			     ""
			     ;; we're closing out an indented section, so how many </ul> do we need?
			     (format "~a</ul>\n" (apply string-append (make-list (- indent tabs) "</ul>\n"))))))
	  ;;(fprintf (current-error-port) "heading: ~s tabs: ~s indent: ~s close-elem: ~s\n" (car lines) tabs indent close-elem)
	  (loop
	    (cdr lines)
	    tabs
	    (format "~a~a~a<ul>\n" acc close-elem (output (string-trim (car lines))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write out a chm file:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main argv)
  (let ((out-path "out.chm")
	(in-dir ".")
	(default-topic "index.html")
	(title "Help File"))
	(args-parse (cdr argv)
		    (section "Options")
		    ((("-o" "--output-file") ?file (help "Name of resulting chm file"))
		     (set! out-path file))
		    ((("-i" "--input") ?dir (help "Directory containing input pages"))
		     (set! in-dir dir))
		    ((("-d" "--default") ?default (help "Default topic filename."))
		     (set! default-topic default))
		    ((("-t" "--title") ?help-title (help "Helpfile title (default 'Help File')."))
		     (set! title help-title))
		    ((("-h" "--help") (help "This message."))
		     (printf "Manumission version ~a\n" *version*)
		     (args-parse-usage #f)
		     (exit)))
    (let ((contents (format "~a/contents.txt" in-dir))
	  (index (format "~a/index.txt" in-dir)))
      (unless (file-exists? contents)
	(printf "Generate contents file (Y/n)? ")
	(let ((response (read-line)))
	  ;; a help file w/o contents is very limited, but we can do it
	  (unless (and (> (string-length response) 0) (eq? (string-ref response 0) #\n))
	    (generate-contents in-dir))))
      (unless (file-exists? index)
	(printf "Generate index file (Y/n)? ")
	(let ((response (read-line)))
	  ;; a help file w/o contents is very limited, but we can do it
	  (unless (and (> (string-length response) 0) (eq? (string-ref response 0) #\n))
	    (generate-index in-dir))))
      (let* ((contents-topic (if (file-exists? contents)
			       (convert-contents contents in-dir) ""))
	     (index-topic (if (file-exists? index)
			    (convert-index index in-dir) ""))
	     (topics (load-dir in-dir default-topic contents-topic index-topic))
	     (titles (map (lambda (t)
			    (let ((title1 (page-title (topic-data t))))
			      (if title1 title1 (ignore-/ (topic-name t)))))
			  (keep-not special? topics)))
	     (string-list 
	       (keep-unique
		 (append (list title contents-topic index-topic)
			 (map (compose ignore-/ topic-name)
			      (keep-not special? topics))
			 titles)))
	     (namelist-data (namelist '("Uncompressed" "MSCompressed")))
	     (idxhdr (idxhdr$ topics))
	     (file-list
	       (append
		 (list
		   (make-topic "::DataSpace/NameList" 0 namelist-data #f)
		   (make-topic "/" 0 #f #f)
		   (make-topic "/#ITBITS" 0 #f #f)
		   (make-topic "/#SYSTEM" 0 (system$ topics title idxhdr) #f)
		   (make-topic "/#IDXHDR" 0 idxhdr #f)
		   (make-topic "/#STRINGS" 0 (strings$ string-list) #f)
		   (make-topic "/#TOPICS" 0 (topics$ topics string-list) #f)
		   (make-topic "/#URLSTR" 0 (urlstr$ topics) #f)
		   (make-topic "/#URLTBL" 0 (urltbl$) #f)
		   (make-topic "/$FIftiMain" 0 #f #f)
		   ;;(make-topic "/#IVB" 0 zero #f)  ; total size is 0
		   (make-topic "/$OBJINST" 0 #f #f) ; not needed
		   (make-topic "/$WWAssociativeLinks/" 0 #f #f)
		   (make-topic "/$WWAssociativeLinks/Property" 0 zero #f)
		   (make-topic "/$WWKeywordLinks/" 0 #f #f)
		   ;;(make-topic "/$WWKeywordLinks/BTree" 0 (btree (format "~a/~a" in-dir index)) #f)
		   (make-topic "/$WWKeywordLinks/Property" 0 zero #f)
		   )
		 topics))
	     (sorted-list (sort file-list (lambda (a b) (string-ci<? (topic-name a) (topic-name b)))))
	     (content-listing
	       (let loop ((lst (append
				 sorted-list
				 ;; we tack on an empty compressed section:
				 (list (make-topic "::DataSpace/Storage/MSCompressed/Content" 0 "" #f))))
			  (offset 0)
			  (acc '()))
		 (if (null? lst)
		   (reverse acc)
		   (let* ((entry (car lst))
			  (data (topic-data entry))
			  (len (if data (string-length data) 0)))
		     (loop (cdr lst)
			   (+ offset len)
			   (cons (list (topic-name entry)
				       (topic-section entry)
				       data
				       (if data offset 0)
				       len)
				 acc))))))
	     (cont-0-data (apply string-append
				 (remove-false
				   (map topic-data sorted-list))))
	     (cont-1-data (lzx-data))
	     (dir-listings (listing-chunks content-listing))
	     (index-chunk-list (indexing-chunk dir-listings))
	     (filesize (+ (string-length (header index-chunk-list dir-listings 0))  ; must call header twice (don't know filesize yet)
			  (string-length cont-0-data) (string-length cont-1-data))))
	(with-output-to-file
	  out-path
	  (lambda ()
	    (display (header index-chunk-list dir-listings filesize))
	    (display cont-0-data)
	    (display cont-1-data)))))))
