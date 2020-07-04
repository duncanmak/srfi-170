;; please see copyright notice in 170/COPYING

(define-library (srfi 170)
  (import (srfi 9)
          (class java.util EnumSet Iterator)
          (class java.nio.file DirectoryStream DirectoryStream$Filter Files Path Paths)
          (class java.nio.file.attribute
                 BasicFileAttributeView
                 FileAttribute
                 PosixFileAttributeView
                 PosixFilePermission
                 PosixFilePermissions)
          (class java.nio.channels FileChannel))
  (export

   ;; 3.1  Errors

   ;; syscall-* imported from SRFI 198
   ;; syscall-error?
   ;; syscall-error:errno syscall-error:message
   ;; syscall-error:procedure-name syscall-error:data

   ;; srfi-170-error?
   ;; srfi-170-error:message
   ;; srfi-170-error:procedure-name srfi-170-error:data


   ;; 3.2  I/O

   open-file
   open/read open/write open/read+write
   open/append open/create open/exclusive open/nofollow open/truncate
   fdes->textual-input-port fdes->binary-input-port
   fdes->textual-output-port fdes->binary-output-port
   port-fdes
   close-fdes


   ;; 3.3  File system

   create-directory create-fifo create-hard-link create-symlink
   read-symlink
   rename-file
   delete-directory
   set-file-mode set-file-owner set-file-group
   set-file-timespecs timespec/now timespec/omit
   truncate-file

   file-info file-info?
   file-info:device file-info:inode file-info:mode file-info:nlinks
   file-info:uid file-info:gid file-info:rdev file-info:size
   ;; file-info:blksize file-info:blocks are below, not in Windows
   file-info:atime file-info:mtime file-info:ctime

   file-info-directory? file-info-fifo? file-info-symlink? file-info-regular?

   directory-files
   make-directory-files-generator
   open-directory read-directory close-directory

   real-path

   temp-file-prefix
   create-temp-file
   ;; call-with-temporary-filename is left as an exercise for the reader


   ;; 3.5  Process state

   umask set-umask!
   current-directory set-current-directory!
   pid parent-pid process-group
   nice

   user-uid user-gid
   user-effective-uid user-effective-gid
   user-supplementary-gids


   ;; 3.6  User and group database access

   user-info user-info?
   user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell
   user-info:full-name user-info:parsed-full-name

   group-info group-info?
   group-info:name group-info:gid


   ;; 3.10  Time

   posix-time monotonic-time


   ;; 3.11  Environment variables

   set-environment-variable!
   delete-environment-variable!


   ;; 3.12  Terminal device control

   terminal?

   )

  (cond-expand ((not windows)
    (export

     ;; 3.3  File system

     file-info:blksize file-info:blocks)))

  (cond-expand
   (chibi
    (import
     (scheme base)
     (scheme case-lambda)
     (only (scheme process-context) get-environment-variable)
     (chibi)
     (chibi optional) ;; Snow package for optional args
     (only (chibi filesystem) file-exists? delete-file open open/write open/create)
     (only (srfi 1) take)
     (only (srfi 8) receive) ;; the only export, but let us maintain form
     (only (srfi 27) random-integer)
     (only (srfi 98) get-environment-variables)
     (only (srfi 115) regexp-replace-all regexp-split)
     (srfi 151) ;; bitwise operators
     (rename (only (srfi 174) timespec timespec? timespec-seconds timespec-nanoseconds)
             (timespec make-timespec))
     (only (srfi 198) syscall-error? syscall-error:errno syscall-error:message syscall-error:procedure-name syscall-error:data errno-error)
     )

    (include-shared "170/170")
    (include-shared "170/aux")))

  (include "170/170.scm"))
