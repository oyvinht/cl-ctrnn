;;;; Hey, Emacs! This is a -*- Lisp -*- file!
(defsystem cl-ctrnn
  :author "Øyvin Halfdan Thuv"
  :name "CL-CTRNN"
  :version 1.0
  :components 
  ((:file "defpackage")
   (:file "cl-ctrnn" :depends-on ("defpackage" "neurons" "synapses"
				  "neuron-and-synapse-methods"))
   (:file "neuron-and-synapse-methods"
    :depends-on ("defpackage" "neurons" "synapses"))
   (:file "neurons"
    :depends-on ("defpackage"))
   (:file "synapses"
    :depends-on ("defpackage"))))

