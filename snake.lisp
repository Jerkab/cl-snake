;; snake.lip -- Jeu du snake
;;
(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "croatoan"))

;; Defining a package is always a good idea
(defpackage #:jsa.game.snake
  (:use #:cl)
  (:export #:main))

;; TODO: Revoir un peu le code pour le simplifier
;; TODO: Utilisation de caracteres speciaux
;; TODO: Gerer la vitesse du jeu
;; TODO: Activation du score
;; TODO: Prendre en compte les points de vie
;; TODO: Gerer le 'resize'

(in-package #:jsa.game.snake)

(defparameter *scr* nil)


(defvar *game-width* 40)
(defvar *game-height* 40)

(defun game (win)

  (let ((snake '((20 3) (20 2) (20 1) (20 0)))
        (direction '(0 1))
        (foods '())
        (size (list (1- *game-width*) (1- *game-height*)))
        (ended 0)
        (cycle 0)
        (score 0)
        (life 0)
        (window-game (make-instance 'crt:window :height *game-height* :width *game-width* :y 0 :x 0 :fgcolor :white :border t))
        (window-info (make-instance 'crt:window :height 10 :width 20 :y 0 :x (1+ *game-width*) :fgcolor :white :border t)))
    (labels ((initialize-game ()
               (setq snake '((20 3) (20 2) (20 1) (20 0)))
               (setq direction '(0 1))
               (setq foods '())
               (setq size (list *game-width* *game-height*))
               (setq ended 0)
               (setq life 0)
               (setq score 0)
               (add-food))

             (display-game ()
               (crt:clear window-game)
               (crt:draw-border window-game)
               (crt:add-string window-game (format nil " Area ") :y 0 :x 2 :fgcolor :white)
               (crt:add-char window-game #\* :y (second (first snake)) :x (caar snake) :fgcolor :green)
               (dolist (b (butlast (cdr snake)))
                 (crt:add-char window-game #\= :y (second b) :x (first b) :fgcolor :green))
               (crt:add-char window-game #\- :y (second (first (last snake))) :x (caar (last snake)) :fgcolor :green)
               (dolist (f foods) (crt:add-char window-game #\# :y (second f) :x (car f) :fgcolor :yellow))
               (crt:refresh window-game))
             (display-info ()
               (crt:clear window-info)
               (crt:draw-border window-info)
               (crt:add-string window-info (format nil " Info ") :y 0 :x 2 :fgcolor :white)
               (crt:add-string window-info (format nil "Cycle: ~D" cycle) :y 1 :x 1)
               (crt:add-string window-info (format nil "Snake: ~D" (length snake)) :y 2 :x 1)
               (crt:add-string window-info (format nil "Foods: ~D" (length foods)) :y 3 :x 1)
               (crt:add-string window-info (format nil "Life:  ~D" life) :y 4 :x 1)
               (crt:add-string window-info (format nil "Score: ~D" score) :y 6 :x 1)
               (if (= ended 1)
                   (crt:add-string window-info "GAME OVER" :y 8 :x 1 :fgcolor :red :bgcolor :black))
               (crt:refresh window-info))

             (move-snake ()
               (let* ((nhead (list (+ (first direction) (caar snake))
                                   (+ (second direction) (second (first snake))))))
                 (setq snake (cons nhead
                                   (if (eat-food nhead)
                                       snake
                                       (butlast snake))))))
             (change-direction (dx dy)
               (setq direction (list dx dy)))
             (collision-snake ()
               (find (car snake) (cdr snake) :test #'equal))
             (collision-border ()
               (let ((x (caar snake))
                     (y (second (first snake))))
                 (when (or (< x 1) (>= x (first size)) (< y 1) (>= y (second size)))
                     t)))
             (add-food () ; random between 1 to size
               (setq foods (cons (list (1+ (random (1- (first size))))
                                       (1+ (random (1- (second size)))))
                                 foods)))
             (eat-food (pos)
               (when (find pos foods :test #'equal)
                 (setq foods (remove pos foods :test #'equal))
                 (add-food)
                 t))
             (game-over ()
               (setq ended 1)
               (crt:add-string window-info "GAME OVER" :y 0 :x 0)))

      ;; Bind Event
      ;; TODO: definir le bind sur la fenetre
      (crt:bind win #\q 'crt:exit-event-loop)
      (crt:bind win #\c (lambda (win event) (crt:clear win)))
      (crt:bind win #\r (lambda (win event) (initialize-game)))
      (crt:bind win #\j (lambda () (change-direction -1 0)))
      (crt:bind win #\l (lambda () (change-direction 1  0)))
      (crt:bind win #\i (lambda () (change-direction 0 -1)))
      (crt:bind win #\k (lambda () (change-direction 0  1)))
      ;; (crt:bind win :left  (lambda () (change-direction -1 0)))
      ;; (crt:bind win :right (lambda () (change-direction 1  0)))
      ;; (crt:bind win :up    (lambda () (change-direction 0 -1)))
      ;; (crt:bind win :down  (lambda () (change-direction 0  1)))
      (crt:bind win nil (lambda (win event)
                          (if (= ended 0)
                            (move-snake))
                          (if (or (= ended 1) (collision-snake) (collision-border))
                              (game-over)
                              (progn
                                (setq cycle (1+ cycle))
                                (display-game)))
                          (display-info)))

      (add-food))))

(defun main ()
  (crt:with-screen (scr
                    :input-echoing nil
                    :input-blocking nil
                    :enable-function-keys t
                    :cursor-visible nil)
    (game scr)
    (setf (crt:frame-rate scr) 10)
    (crt:run-event-loop (setf *scr* scr))))
(main)
