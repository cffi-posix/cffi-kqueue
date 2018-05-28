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
  (let ((fd (c-kqueue)))
    (when (< fd 0)
      (error-errno "kqueue"))
    fd))

(defcfun ("kevent" c-kevent) :int
  (kq :int)
  (changelist (:pointer (:struct kevent)))
  (nchanges :int)
  (eventlist (:pointer (:struct kevent)))
  (nevents :int)
  (timeout (:pointer (:struct timespec))))

(defun kevent-ident (kev)
  (foreign-slot-value kev '(:struct kevent) 'ident))

(defun kevent-filter (kev)
  (foreign-slot-value kev '(:struct kevent) 'filter))

(defun kevent-flags (kev)
  (foreign-slot-value kev '(:struct kevent) 'flags))

(defun kevent-fflags (kev)
  (foreign-slot-value kev '(:struct kevent) 'fflags))

(defun kevent-data (kev)
  (foreign-slot-value kev '(:struct kevent) 'data))

(defun kevent-udata (kev)
  (foreign-slot-value kev '(:struct kevent) 'udata))

(defun kevent (kq &key changes n-changes
                    on-event (max-events 10)
                    (timeout 0))
  (with-foreign-objects ((timespec '(:struct timespec))
                         (events '(:struct kevent) max-events))
    (setf (foreign-slot-value timespec '(:struct timespec) 'tv-sec)
          (floor timeout)
          (foreign-slot-value timespec '(:struct timespec) 'tv-nsec)
          (floor (- timeout (floor timeout)) 1/1000000000))
    (let ((n (c-kevent kq changes n-changes events max-events timespec)))
      (when (< n 0)
        (error-errno "kevent"))
      (when on-event
        (dotimes (i n)
          (funcall on-event (mem-aref events '(:struct kevent) i)))))))
