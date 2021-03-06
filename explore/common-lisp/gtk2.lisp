;; (ql:quickload 'cl-gtk2-gtk)
;; (asdf:operate 'asdf:load-op 'cl-gtk2-gtk)
(require 'cl-gtk2-gtk)

(defun run ()
  (let ((output *standard-output*))
    (gtk:within-main-loop
     (let ((window (make-instance 'gtk:gtk-window
                                  :type :toplevel
                                  :window-position :center
                                  :title "Hello world!"
                                  :default-width 300
                                  :default-height 100))
           (button (make-instance 'gtk:button :label "Hello, world!"))
           (counter 0))
       (gobject:g-signal-connect button "clicked"
                                 (lambda (b)
                                   (declare (ignore b))
                                   (format output "Hello, world!~%")
                                   (setf (gtk:button-label button)
                                         (format nil
                                                 "Hello, world! (clicked ~D times)"
                                                 (incf counter)))))
       (gtk:container-add window button)
       (gtk:widget-show window :all t)))
    (gtk:gtk-main)))

(defun demo-class-browser ()
  (let ((output *standard-output*))
    (gtk:within-main-loop
     (let* ((window (make-instance 'gtk:gtk-window
                                   :window-position :center
                                   :title "Class Browser"
                                   :default-width 400
                                   :default-height 600))
            (search-entry (make-instance 'gtk:entry))
            (search-button (make-instance 'gtk:button :label "Search"))
            (scroll (make-instance 'gtk:scrolled-window
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :automatic))
            (slots-model (make-instance 'gtk:array-list-store))
            (slots-list (make-instance 'gtk:tree-view :model slots-model)))
       (let ((v-box (make-instance 'gtk:v-box))
             (search-box (make-instance 'gtk:h-box)))
         (gtk:container-add window v-box)
         (gtk:box-pack-start v-box search-box :expand nil)
         (gtk:box-pack-start search-box search-entry)
         (gtk:box-pack-start search-box search-button :expand nil)
         (gtk:box-pack-start v-box scroll)
         (gtk:container-add scroll slots-list))
       (gtk:store-add-column slots-model "gchararray"
                             (lambda (slot)
                               (format nil "~S" (closer-mop:slot-definition-name slot))))
       (let ((col (make-instance 'gtk:tree-view-column :title "Slot name"))
             (cr (make-instance 'gtk:cell-renderer-text)))
         (gtk:tree-view-column-pack-start col cr)
         (gtk:tree-view-column-add-attribute col cr "text" 0)
         (gtk:tree-view-append-column slots-list col))
       (labels ((display-class-slots (class)
                  (format output "Displaying ~A~%" class)
                  (loop
                     repeat (gtk:store-items-count slots-model)
                     do (gtk:store-remove-item slots-model (gtk:store-item slots-model 0)))
                  (closer-mop:finalize-inheritance class)
                  (loop
                     for slot in (closer-mop:class-slots class)
                     do (gtk:store-add-item slots-model slot)))
                (on-search-clicked (button)
                  (declare (ignore button))
                  (gtk:with-gtk-message-error-handler
                      (let* ((class-name (read-from-string (gtk:entry-text search-entry)))
                             (class (find-class class-name)))
                        (display-class-slots class)))))
         (gobject:g-signal-connect search-button "clicked" #'on-search-clicked))
       (gtk:widget-show window)))))

(defun build ()
  #+sbcl
  (sb-ext:save-lisp-and-die "gtk-example.exe"
                            :executable t
                            :purify t
                            :toplevel #'run)
  #+ecl
  (progn
    (require 'asdf)
    (asdf:defsystem #:gtk-example
      :serial t
      :depends-on (#:cl-gtk2-gtk)
      :components ((:file "gtk2")))
    (asdf:make-build :gtk-example :type :fasb))) ;; fasb or program

;; (run)
