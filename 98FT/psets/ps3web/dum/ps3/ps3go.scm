;;;; setup PS3

;;; Make three windows

;;;This magic is here so we'll make sure that the g's get defined the
;;;first time the file is loaded, but that the windows will get closed
;;;if the file is subsequently loaded.

(if (lexical-unreferenceable? user-initial-environment 'g1)
    (define g1 false))
(if (lexical-unreferenceable? user-initial-environment 'g2)
    (define g2 false))
(if (lexical-unreferenceable? user-initial-environment 'g3)
    (define g3 false))

(define (setup-windows)
  (let* ((name (graphics-type-name (graphics-type #f)))
	 (xoffset
	  (case name
	    ((X) -10)
	    ((WIN32) 0)
	    ((OS/2) 0)
	    (else
	     (error "Unsupported graphics type:" name))))
	 (small-windows-screen?
	  (and (eq? name 'win32) (< (win32-screen-height) 650))))

    (if (and g1 (graphics-device? g1)) (graphics-close g1))
    (set! g1 (make-window 256 256 xoffset +10))
    (graphics-set-coordinate-limits g1 0.0 0.0 1.0 1.0)
    (graphics-operation g1 'set-window-name "Graphics: g1")

    (if (and g2 (graphics-device? g2)) (graphics-close g2))
    (set! g2 (make-window 256 256 xoffset +320))
    (graphics-set-coordinate-limits g2 0.0 0.0 1.0 1.0)
    (graphics-operation g2 'set-window-name "Graphics: g2")

    (if (and g3 (graphics-device? g3)) (graphics-close g3))
    (set! g3 (make-window 256
			  256
			  (if small-windows-screen?
			      (+ xoffset 50)
			      xoffset)
			  (if small-windows-screen?
			      60
			      630)))
    (graphics-set-coordinate-limits g3 0.0 0.0 1.0 1.0)
    (graphics-operation g3 'set-window-name "Graphics: g3")))


(define 6001-image-directory
  (->namestring (system-library-pathname "6001/images/")))

(define (compiled-code name)
  (let ((architecture
	 (if (and (eq? 'UNIX microcode-id/operating-system)
		  (string-ci=? "HP-UX" microcode-id/operating-system-variant))
	     "hpux"
	     "i86")))
    (system-library-pathname (string-append "6001/bin/" architecture "/" name))))

(load (compiled-code "prmpnt.com"))

;;We can't load hend.scm until the basic data stuctures have been defined, so
;;we only open a buffer for the file when we load ps3go, and wait until evaluating
;;load to actally load the code in hend.scm

((access find-file-noselect (->environment '(edwin)))
  (u6001-dir "psets/ps3/hend.scm")
  #t)

((access find-file-noselect (->environment '(edwin)))
  (u6001-dir "psets/ps3/ps3-hend.scm")
  #t)

(define (setup)
  (setup-windows)
  (load (u6001-dir "psets/ps3/ps3-hend"))
  (load (u6001-dir "psets/ps3/hend")))


'done



