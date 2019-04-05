;;
;;  cffi-kqueue  -  Common Lisp wrapper for BSD kqueue syscall
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cffi-kqueue)

(defcfun ("kqueue" c-kqueue) :int)

(defun kqueue ()
  (let ((fd (the (signed-byte 32) (c-kqueue))))
    (when (< fd 0)
      (error-errno "kqueue"))
    fd))

(defmacro with-kqueue ((kq) &body body)
  (let ((fd (gensym "FD-")))
    `(let ((,fd (kqueue)))
       (declare (type unistd:file-descriptor ,fd))
       (unwind-protect (let ((,kq ,fd))
                         (declare (type unistd:file-descriptor ,kq))
                         ,@body)
         (unistd:close ,fd)))))

(defmacro kevent-ident (kev)
  `(foreign-slot-value ,kev '(:struct kevent) 'ident))

(defmacro kevent-filter (kev)
  `(foreign-slot-value ,kev '(:struct kevent) 'filter))

(defmacro kevent-flags (kev)
  `(foreign-slot-value ,kev '(:struct kevent) 'flags))

(defmacro kevent-fflags (kev)
  `(foreign-slot-value ,kev '(:struct kevent) 'fflags))

(defmacro kevent-data (kev)
  `(foreign-slot-value ,kev '(:struct kevent) 'data))

(defmacro kevent-udata (kev)
  `(foreign-slot-value ,kev '(:struct kevent) 'udata))

(defcfun ("kevent" c-kevent) :int
  (kq :int)
  (changelist (:pointer (:struct kevent)))
  (nchanges :int)
  (eventlist (:pointer (:struct kevent)))
  (nevents :int)
  (timeout (:pointer (:struct timespec))))

(defun seconds-to-timespec (timespec seconds)
  (let* ((sec (floor seconds))
         (nanosec (floor (- seconds sec) 1/1000000000)))
    (declare (type fixnum sec nanosec))
      (with-foreign-slots ((tv-sec tv-nsec) timespec (:struct timespec))
        (setf tv-sec sec tv-nsec nanosec))))

(defun kevent (kq &key changes n-changes events n-events timeout)
  (setf changes (or changes (null-pointer))
        n-changes (or n-changes 0)
        events (or events (null-pointer))
        n-events (or n-events 0)
        timeout (or timeout (null-pointer)))
  (let ((n (c-kevent kq changes n-changes events n-events timeout)))
    (declare (type (signed-byte 32) n))
    (when (< n 0)
      (error-errno "kevent"))
    n))
