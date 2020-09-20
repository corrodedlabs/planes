
(library-directories '("./lib/chez-gl/lib/"
		       "./lib/prelude/thunderchez"
		       "./lib/prelude/"
		       "./lib/chez-sdl/lib/"))

(import (chezscheme)
	(gl)
	(sdl))


;; Startup SDL 2 and request a window suitable
;; for requesting an OpenGL context
(sdl-init SDL-INIT-VIDEO
	  SDL-INIT-EVENTS)

(define *window*
  (sdl-create-window "chezscheme"
		     SDL-WINDOWPOS-UNDEFINED
		     SDL-WINDOWPOS-UNDEFINED
		     640
		     480
		     SDL-WINDOW-OPENGL))

;; Setting hints for a 32-bit color depth OpenGL context
;; with a double buffered framebuffer and a core 3.3 context.
(sdl-gl-set-attribute! SDL-GL-RED-SIZE   8)
(sdl-gl-set-attribute! SDL-GL-GREEN-SIZE 8)
(sdl-gl-set-attribute! SDL-GL-BLUE-SIZE  8)
(sdl-gl-set-attribute! SDL-GL-ALPHA-SIZE 8)

(sdl-gl-set-attribute! SDL-GL-DOUBLEBUFFER 1)

(sdl-gl-set-attribute! SDL-GL-CONTEXT-PROFILE-MASK
		       SDL-GL-CONTEXT-PROFILE-CORE)
(sdl-gl-set-attribute! SDL-GL-CONTEXT-MAJOR-VERSION 3)
(sdl-gl-set-attribute! SDL-GL-CONTEXT-MINOR-VERSION 3)

(define *gl-context*
  (sdl-gl-create-context *window*))

;; Try to use adaptive vsync.
;; Fallback to regular vsync if unavailable.
(if (= (sdl-gl-set-swap-interval! -1) -1)
    (sdl-gl-set-swap-interval! 1))

;;
(define (should-run?)
  (sdl-poll-event)
  (cond ((sdl-event-none?) #t)
	((sdl-event-quit?) #f)
	(else
	 (should-run?))))

;;
(define (loop-with-time proc)
  (define (loop t)
    (let ((start-time (sdl-get-performance-counter)))
      (proc t)
      (if (should-run?)
	  (let* ((end-time   (sdl-get-performance-counter))
		 (delta-time (/ (- end-time start-time)
				(sdl-get-performance-frequency))))
	    (loop (+ t delta-time))))))
  (loop 0.0))


(define-record-type shader
  (nongenerative)
  (fields vertex fragment))

(define (compile-shader shader-source shader-type)
  (let* ((shader-type (case shader-type
			((vertex-shader) GL-VERTEX-SHADER)
			((fragment-shader) GL-FRAGMENT-SHADER)
			(else (error "shader type not supported" shader-type))))
	 (shader (gl-create-shader shader-type)))
    (dynamic-wind
	(lambda () #f)
	(lambda ()
	  (gl-shader-source shader shader-source)
	  (gl-compile-shader shader)
	  shader)
	(lambda ()
	  (gl-delete-shader shader)))))

(define (shaders->program shader-data)
  (let* ((fragment-shader (compile-shader (shader-vertex shader-data)
					  'vertex-shader))
	 (vertex-shader (compile-shader (shader-fragment shader-data)
					'fragment-shader))
	 (program (gl-create-program)))
    (gl-attach-shader program vertex-shader)
    (gl-attach-shader program fragment-shader)
    (gl-link-program program)
    (gl-use-program program)
    program))


;;
(gl-init)

;;
(gl-clear-color 1.0 1.0 1.0 1.0)
(gl-viewport 0 0 640 480)


;;
(define vertex-shader-source
  "#version 330 core

layout (location = 0) in vec3 pos;

void main()
{
  gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
}")

(define fragment-shader-source
  "#version 330 core

out vec4 frag;

void main()
{
    frag = vec4(1.0f, 0.0f, 1.0f, 1.0f);
}")

(define program (shaders->program (make-shader vertex-shader-source
					       fragment-shader-source)))

;;
(define gl-vao (gl-gen-vertex-arrays 1))
(define gl-vbo (gl-gen-buffers 1))

(define bind-buffer
  (lambda ()
    (gl-bind-vertex-array gl-vao)
    (gl-bind-buffer GL-ARRAY-BUFFER gl-vbo)
    (gl-buffer-data GL-ARRAY-BUFFER
		    '(0.5 -0.5 0.0 -0.5 -0.5 0.0 0.0  0.5 0.0)
		    GL-STATIC-DRAW)

    (gl-vertex-attrib-pointer 0 3 GL-FLOAT GL-FALSE (* 3 4) 0)
    (gl-enable-vertex-attrib-array 0)))

(bind-buffer)

(define run-app
  (lambda ()
    (dynamic-wind
	(lambda () (newline))
	(lambda ()
	  (loop-with-time (lambda (t)
			    (gl-clear-color (sin t) 0.5 (cos t) 1.0)
			    (gl-clear GL-COLOR-BUFFER-BIT)
			    (gl-draw-arrays GL-TRIANGLES 0 3)
			    (sdl-gl-swap-window *window*))))
	(lambda ()
	  (gl-delete-program program)
	  (gl-delete-vertex-arrays gl-vao)
	  (gl-delete-buffers gl-vbo)))))

(run-app)

(sdl-gl-delete-context *gl-context*)
(sdl-destroy-window *window*)
(sdl-quit)
