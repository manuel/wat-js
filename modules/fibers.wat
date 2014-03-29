;; -*- scheme -*-

(defmodule fibers (new-channel channel-send channel-receive go)

  (def fiber-prompt 'fiber-prompt)
  
  (defprototype Channel Object
    (buffer
     full
     senders
     receivers))
  
  (def (new-channel)
    (new Channel #null #f (array) (array)))
  
  (def (channel-send (ch Channel) msg)
    (label done
      (loop
        (label retry
          (if (.full ch)
              (begin
                (take-subcont fiber-prompt k
                  (~push (.senders ch) k))
                (retry))
              (begin
                (set (.buffer ch) msg)
                (set (.full ch) #t)
                (if (> (.length (.receivers ch)) 0)
                    (let ((receiver (~shift (.receivers ch))))
                      (schedule-fiber receiver))
                    (begin
                      (take-subcont fiber-prompt k
                        (~push (.senders ch) k))
                      (done)))))))))

  (def (channel-receive (ch Channel))
    (label done
      (loop
        (label retry
          (if (.full ch)
              (begin
                (let ((msg (.buffer ch))
                      (sender (~shift (.senders ch))))
                  (set (.buffer ch) #null)
                  (set (.full ch) #f)
                  (schedule-fiber sender)
                  (done msg)))
              (begin
                (take-subcont fiber-prompt k
                  (~push (.receivers ch) k))
                (retry)))))))

  (def (schedule-fiber k)
    (def (callback . #ignore)
      (push-prompt fiber-prompt
        (push-subcont k)))
    ($setTimeout (js-callback callback) 0))

  (def (go fun)
    (push-prompt fiber-prompt
      (fun)))

)

(import fibers (new-channel channel-send channel-receive go))

(def (pinger ch)
  (loop
    (channel-send ch "ping")))

(def (printer ch)
  (loop
    (log (channel-receive ch))))

(let ((ch (new-channel)))
  (go (lambda () (pinger ch)))
  (go (lambda () (printer ch))))
