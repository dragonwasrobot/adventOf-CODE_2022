;; day7-solution.el

(defconst day7-puzzle-input (s-lines (s-chomp (f-read "day7-input.txt"))))

(defun day7-go-in-one-level (cwd dir)
  "Adds a new dir to 'cwd': '/foo/bar' and 'baz' becomes '/foo/bar/baz'."
  (concat cwd dir))

(defun day7-go-out-one-level (cwd)
  "Removes the last part of 'cwd': /foo/bar/baz becomes /foo/bar.

The implementation could be more elegant."
  (let ((new-cwd (->> cwd
                    (s-chop-suffix "/")
                    (reverse)
                    (s-split "/")
                    (cdr)
                    (s-join "/")
                    (reverse)
                    )))
    (if (equal new-cwd "") "/" (concat new-cwd "/"))))

(defun day7-parse-commands (lines)
  "Parses the command history input"
  (reverse (cdr (cl-reduce (lambda (acc line)
                             (let ((cwd (car acc))
                                   (file-sizes (cdr acc)))
                               (cond
                                ;; cd <dir-name>
                                ((s-starts-with? "$ cd" line)
                                 (let ((dir-name (nth 2 (s-split " " line))))
                                   (cond ((equal "/" dir-name) (cons "/" file-sizes))

                                         ((equal ".." dir-name) (cons (day7-go-out-one-level cwd)
                                                                     file-sizes))

                                         (t (cons
                                             (day7-go-in-one-level cwd (concat dir-name "/"))
                                             file-sizes)))))

                                ;; ls
                                ((s-starts-with? "$ ls" line) (cons cwd file-sizes))

                                ;; dir <dir-name>
                                ((s-starts-with? "dir" line)
                                 (let ((dir-name (nth 1 (s-split " " line))))
                                   (cons cwd
                                         (cons (list :dir (concat cwd dir-name)) file-sizes))))

                                ;; <size> <file-name>
                                (t (let ((file-size (nth 0 (s-split " " line)))
                                         (file-name (nth 1 (s-split " " line))))
                                     (cons cwd
                                           (cons (list :file
                                                       (concat cwd file-name)
                                                       (string-to-number file-size))
                                                 file-sizes))))
                                )))
                           lines
                           :initial-value '("" . ())))))

(defun day7-dirname (path)
  (->> path
     (s-split "/")
     (reverse)
     (cdr)
     (reverse)
     (s-join "/")
     ))

(defun day7-calculate-directory-sizes (file-sizes)
  (let* ((directories (seq-filter (lambda (f) (equal (car f) :dir)) file-sizes))

         (files (seq-filter (lambda (f) (equal (car f) :file)) file-sizes))

         (root-size (cl-reduce (lambda (acc f) (+ (nth 2 f) acc)) files :initial-value 0))

         (directory-sizes
          (seq-map (lambda (d)
                     (let* ((dir-path (nth 1 d))
                            (dir-size
                             (cl-reduce (lambda (acc file) (+ (nth 2 file) acc))
                                        (seq-filter (lambda (file) (equal (day7-dirname (nth 1 file))
                                                                     dir-path))
                                                    files)
                                        :initial-value 0)))
                       (list :dir dir-path dir-size)))
                   directories))

         (final-directory-sizes
          (let ((sorted-by-size
                 (seq-sort-by (lambda (e) (nth 1 e)) (lambda (e1 e2) (not (s-less? e1 e2))) directory-sizes)))

            (cl-reduce (lambda (acc dir)
                         (let* ((dir-path (nth 1 dir))
                                (dir-size (nth 2 dir))
                                (subdir-sizes (cl-reduce
                                               (lambda (total dir) (+ (nth 2 dir) total))
                                               (seq-filter (lambda (dir)
                                                             (and (not (equal dir-path (nth 1 dir)))
                                                                  (equal (day7-dirname (nth 1 dir))
                                                                         dir-path)))
                                                           acc)
                                               :initial-value 0)))
                           (cons (list :dir dir-path (+ dir-size subdir-sizes)) acc)))
                       sorted-by-size
                       :initial-value '()))))

    (cons root-size final-directory-sizes)))

(defun day7-parse-and-calculate-directory-sizes (trace-input)
  (->> trace-input
     day7-parse-commands
     (seq-sort-by (lambda (e) (nth 1 e)) (lambda (p1 p2) (s-less? p1 p2)))
     day7-calculate-directory-sizes
     ))

;; Answer 1

(defun day7-part1 (trace-input)
  (cl-reduce (lambda (acc dir)
               (let ((dir-size (nth 2 dir)))
                 (if (<= dir-size 100000) (+ acc dir-size) acc)))
             (cdr (day7-parse-and-calculate-directory-sizes trace-input))
             :initial-value 0))

(day7-part1 day7-puzzle-input) ;; 1_444_896

;; Answer 2

(defun day7-part2 (trace-input)
  (let* ((disk-space 70000000)
         (unused-space-target 30000000)
         (root-and-directory-sizes (day7-parse-and-calculate-directory-sizes trace-input))
         (root-size (car root-and-directory-sizes))
         (directory-sizes (cdr root-and-directory-sizes))
         (unused-space (- disk-space root-size))
         (target-size (- 30000000 unused-space)))

    (->> directory-sizes
       (seq-map (lambda (dir) (let ((dir-size (nth 2 dir))) (cons dir-size (- target-size dir-size)))))
       (seq-filter (lambda (pair) (<= (cdr pair) 0)))
       (-max-by (lambda (e1 e2) (>= (cdr e1) (cdr e2)))))))

(day7-part2 day7-puzzle-input) ;; 404_395
