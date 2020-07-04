(define (bits->permissions bits)
  (let ((result (EnumSet:noneOf PosixFilePermission:class)))
    (for-each (lambda (permission ::PosixFilePermission)
                (unless (zero? (bitwise-and (bitwise-arithmetic-shift-left 1 (permission:ordinal)) bits))
                  (result:add permission)))
              (PosixFilePermission:values))
    result))

(define (create-directory (fname ::string) #!optional (permission-bits #o755))
  (let ((dir ::Path (Paths:get fname))
        (perms ::FileAttribute (PosixFilePermissions:asFileAttribute
                                (bits->permissions permission-bits))))
    (Files:createDirectory dir perms)
    #!void))

(define (create-hard-link (oldname ::string) (newname ::string))
  (Files:createLink (Paths:get oldname) (Paths:get newname))
  #!void)

(define (create-symlink (oldname ::string) (newname ::string))
  (Files:createSymbolicLink (Paths:get oldname) (Paths:get newname))
  #!void)

(define (read-link (fname ::string))
  ((Files:readSymbolicLink (Paths:get fname)):tostring)
  #!void)

(define (rename-file (oldname ::string) (newname ::string))
  (let ((source ::Path (Paths:get oldname)))
    (Files:move source (source:resolveSibling newname))
    #!void))

(define (delete-directory (fname ::string))
  (let ((dir ::Path (Paths:get fname)))
    (if (and (Files:exist dir) (Files:isDirectory dir))
        (Files:delete dir)
        (error "not a directory"))))

(define (set-file-mode (fname ::string) (permission-bits ::int))
  (let ((file ::Path (Paths:get fname))
        (perms (bits->permissions bits)))
    (file:setPosixFilePermissions perms)
    #!void))

(define (set-file-owner (fname ::string) uid)
  (Files:setAttribute (Paths:get fname) "posix:owner" uid))

(define (set-file-group (fname ::string) gid)
  (Files:setAttribute (Paths:get fname) "posix:group" gid))

(define (truncate-file (fname ::string) (len ::long))
  (let ((channel ::FileChannel (FileChannel:open (Paths:get fname))))
    (channel:truncate len)))

(define-record-type file-info
  (make-file-info attrs)
  file-info?
  (attrs file-info:attributes))

(define (file-info (fname ::string) (follow? ::boolean))
  (let ((attrs (Files:readAttributes (Paths:get fname) "unix:*")))
    (make-file-info attrs)))

(define (%file-info:get (info ::file-info) (key ::string))
  (let ((attrs ::Map (file-info:attrs info)))
    (attrs:get (key:toString))))

(define (file-info:device (info ::file-info)) (%file-info:get info "dev"))
(define (file-info:inode (info ::file-info))  (%file-info:get info "dev"))
(define (file-info:mode (info ::file-info))   (%file-info:get info "mode"))
(define (file-info:nlinks (info ::file-info)) (%file-info:get info "nlink"))
(define (file-info:uid (info ::file-info))    (%file-info:get info "uid"))
(define (file-info:gid (info ::file-info))    (%file-info:get info "gid"))
(define (file-info:rdev (info ::file-info))   (%file-info:get info "rdev"))
(define (file-info:atime (info ::file-info))  (%file-info:get info "lastAccessTime"))
(define (file-info:mtime (info ::file-info))  (%file-info:get info "lastModifiedTime"))
(define (file-info:ctime (info ::file-info))  (%file-info:get info "creationTime"))

(define (file-info-directory? (info ::file-info) (%file-info:get info "isDirectory")))
(define (file-info-symlink? (info ::file-info)   (%file-info:get info "isSymbolicLink")))
(define (file-info-regular? (info ::file-info)   (%file-info:get info "isRegular")))

(define (path->string (p ::Path)) ((p:getFileName):toString))

(define (iterator->list (iter ::Iterator) func)
  (let loop ((result '()))
    (if (iter:hasNext)
        (loop (cons (func (iter:next)) result))
        (reverse result))))

(define-record-type directory-object
  (make-directory-object stream iterator)
  directory-object?
  (stream   directory-object:stream)
  (iterator directory-object:iterator directory-object:set-iterator!))

(define (open-directory (dir ::string) #!optional (dot-files? #f)) ::directory-object
  (let* ((filter (object (DirectoryStream$Filter)
                        ((accept (p ::Path))
                         (or dot-files?
                             (not (char=? #\. ((path->string p) 0)))))))
         (stream (Files:newDirectoryStream (Paths:get dir) filter)))
    (make-directory-object stream #f)))

(define (read-directory (dir ::directory-object))
  (unless (directory-object:iterator dir)
    (let ((stream ::DirectoryStream (directory-object:stream dir)))
      (directory-object:set-iterator! dir (stream:iterator))))
  (let* ((iterator ::Iterator (directory-object:iterator dir)))
    (if (iterator:hasNext)
        (let ((entry ::Path (iterator:next)))
          (path->string (entry:getFileName)))
        #!eof)))

(define (close-directory (dir ::directory-object))
  (let ((stream ::DirectoryStream (directory-object:stream dir)))
    (stream:close)))

(define (directory-files (dir ::string) #!optional (dot-files? #f))
  (let ((dir (open-directory dir dot-files?)))
    (unless (directory-object:iterator dir)
      (let ((stream ::DirectoryStream (directory-object:stream dir)))
        (directory-object:set-iterator! dir (stream:iterator))))
    (iterator->list (directory-object:iterator dir) path->string)))

(define (real-path (path ::string))
  (let ((p ::Path(Paths:get path)))
    ((p:toRealPath)):toString))

(define temp-file-prefix
  (make-parameter
   (let* ((proc ::ProcessHandle (ProcessHandle:current))
          (pid  ::string (format #f "~A" (proc:pid))))
     (cond
      ((get-environment-variable "TMPDIR") => (lambda (tmp) ((Paths:get tmp pid):toString)))
      (else ((Paths:get "/" "tmp" pid):toString))))))


(define (create-temp-file #!optional (prefix ::string #!null))
  (let* ((proc ::ProcessHandle (ProcessHandle:current))
         (pid ::string (format #f "~A" (proc:pid))))
    (Files:createTempFile pid #!null
                          (PosixFilePermissions:asFileAttribute
                           (bits->permissions #o600)))))

(define (call-with-temporary-filename maker #!optional prefix)
  (let loop ((result (maker (create-temp-file prefix))))
    (if result
        (loop (maker (create-temp-file prefix))))))
