(in-package :ctrnn)

(defclass neural-network ()
  ((neurons
    :accessor neural-network-neurons
    :initarg :neurons
    :initform nil))
  (:documentation "A network containing a bunch of neurons."))

(defmethod add-neuron! ((network neural-network)(neuron neuron))
  "Destructively modify (add to) the neurons in a network."
  (push neuron (neural-network-neurons network)))

(defmethod synchronously-update-membrane-potentials! ((neural-network neural-network))
  "(Destructively) update membrane potentials in the network."
  ;; First, calculate all firing frequency at this point in time, and store them
  (loop for neuron in (neural-network-neurons neural-network)
        do (setf (neuron-snapshot-firing-frequency neuron)
                 (firing-frequency neuron)))
  ;; Now, using the snapshot frequency, update all membrane potentials
  (loop for neuron in (neural-network-neurons neural-network)
        sum (let ((energy (update-membrane-potential! neuron)))
              energy)))
