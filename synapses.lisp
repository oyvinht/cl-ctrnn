(in-package :ctrnn)

(defclass synapse ()
  ((from-neuron
    :accessor synapse-from-neuron
    :documentation "The neuron that this synapse comes from."
    :initarg :from-neuron
    :initform nil)
   (strength
    :accessor synapse-strength
    :documentation "The weight of this synapse."
    :initarg :strength
    :initform 1.0))
  (:documentation "Weighted onnection between two neurons."))
