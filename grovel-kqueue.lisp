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

(include "sys/types.h")
(include "sys/event.h")
(include "sys/time.h")

(constant (+ev-add+        "EV_ADD"))
(constant (+ev-enable+     "EV_ENABLE"))
(constant (+ev-disable+    "EV_DISABLE"))
(constant (+ev-dispatch+   "EV_DISPATCH"))
(constant (+ev-delete+     "EV_DELETE"))
(constant (+ev-receipt+    "EV_RECEIPT"))
(constant (+ev-oneshot+    "EV_ONESHOT"))
(constant (+ev-clear+      "EV_CLEAR"))
(constant (+ev-eof+        "EV_EOF"))
(constant (+ev-error+      "EV_ERROR"))

(constant (+evfilt-read+   "EVFILT_READ"))
(constant (+evfilt-write+  "EVFILT_WRITE"))
(constant (+evfilt-vnode+  "EVFILT_VNODE"))

(constant (+note-delete+   "NOTE_DELETE"))
(constant (+note-write+    "NOTE_WRITE"))
(constant (+note-extend+   "NOTE_EXTEND"))
(constant (+note-truncate+ "NOTE_TRUNCATE"))
(constant (+note-attrib+   "NOTE_ATTRIB"))
(constant (+note-link+     "NOTE_LINK"))
(constant (+note-rename+   "NOTE_RENAME"))
(constant (+note-revoke+   "NOTE_REVOKE"))

(constant (+evfilt-proc+   "EVFILT_PROC"))

(constant (+note-exit+     "NOTE_EXIT"))
(constant (+note-fork+     "NOTE_FORK"))
(constant (+note-exec+     "NOTE_EXEC"))
(constant (+note-track+    "NOTE_TRACK"))
(constant (+note-trackerr+ "NOTE_TRACKERR"))

(constant (+evfilt-signal+ "EVFILT_SIGNAL"))
(constant (+evfilt-timer+  "EVFILT_TIMER"))
(constant (+evfilt-device+ "EVFILT_DEVICE"))

(ctype uintptr-t "uintptr_t")
(ctype u-short "u_short")
(ctype u-int "u_int")
(ctype int64-t "int64_t")

(cstruct kevent "struct kevent"
  (ident  "ident"  :type uintptr-t)
  (filter "filter" :type :short)
  (flags  "flags"  :type u-short)
  (fflags "fflags" :type u-int)
  (data   "data"   :type int64-t)
  (udata  "udata"  :type :pointer))

(ctype time-t "time_t")

(cstruct timespec "struct timespec"
  (tv-sec "tv_sec" :type time-t)
  (tv-nsec "tv_nsec" :type :long))
