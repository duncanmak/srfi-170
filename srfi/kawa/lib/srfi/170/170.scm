(define-record-type file-info
  (make-file-info device inode mode nLinks uid gid ))

(define (bits->permissions bits)
  (let ((result (EnumSet:noneOf PosixFilePermission:class)))
    (for-each (lambda (permission ::PosixFilePermission)
                (unless (zero? (bitwise-and (bitwise-arithmetic-shift-left 1 (permission:ordinal)) bits))
                  (result:add permission)))
              (PosixFilePermission:values))
    result))

(define (create-directory (fname ::String) #!optional (permission-bits #o755))
  (let ((dir ::Path (Paths:get fname))
        (perms ::FileAttribute (PosixFilePermissions:asFileAttribute
                                (bits->permissions permission-bits))))
    (Files:createDirectory dir perms)
    #!void))

(define (create-hard-link (oldname ::String) (newname ::String))
  (Files:createLink (Paths:get oldname) (Paths:get newname))
  #!void)

(define (create-symlink (oldname ::String) (newname ::String))
  (Files:createSymbolicLink (Paths:get oldname) (Paths:get newname))
  #!void)

(define (read-link (fname ::String))
  ((Files:readSymbolicLink (Paths:get fname)):toString)
  #!void)

(define (rename-file (oldname ::String) (newname ::String))
  (let ((source ::Path (Paths:get oldname)))
    (Files:move source (source:resolveSibling newname))
    #!void))

(define (delete-directory (fname ::String))
  (let ((dir ::Path (Paths:get fname)))
    (if (and (Files:exist dir) (Files:isDirectory dir))
        (Files:delete dir)
        (error "not a directory"))))

(define (set-file-mode (fname ::String) (permission-bits ::int))
  (let ((file ::Path (Paths:get fname))
        (perms (bits->permissions bits)))
    (file:setPosixFilePermissions perms)
    #!void))

(define (set-file-owner (fname ::String) uid)
  (Files:setAttribute (Paths:get fname) "posix:owner" uid))

(define (set-file-group (fname ::String) gid)
  (Files:setAttribute (Paths:get fname) "posix:group" gid))

(define (truncate-file (fname ::String) (len ::long))
  (let ((channel ::FileChannel (FileChannel:open (Paths:get fname))))
    (channel:truncate len)))

