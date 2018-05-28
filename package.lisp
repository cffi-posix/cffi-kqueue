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

(in-package :common-lisp)

(defpackage :cffi-kqueue
  (:nicknames :kqueue)
  (:use
   :cffi
   :common-lisp
   :errno)
  (:shadow
   #:mod)
  (:export
   #:+ev-add+
   #:+ev-enable+
   #:+ev-disable+
   #:+ev-dispatch+
   #:+ev-delete+
   #:+ev-receipt+
   #:+ev-oneshot+
   #:+ev-clear+
   #:+ev-eof+
   #:+ev-error+
   #:+evfilt-read+
   #:+evfilt-write+
   #:+evfilt-vnode+
   #:+note-delete+
   #:+note-write+
   #:+note-extend+
   #:+note-truncate+
   #:+note-attrib+
   #:+note-link+
   #:+note-rename+
   #:+note-revoke+
   #:+evfilt-proc+
   #:+note-exit+
   #:+note-fork+
   #:+note-exec+
   #:+note-track+
   #:+note-trackerr+
   #:+evfilt-signal+
   #:+evfilt-timer+
   #:+evfilt-device+
   #:c-kevent
   #:c-kqueue
   #:kevent
   #:kqueue
   #:ev-set))
