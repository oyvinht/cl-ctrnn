(in-package :ctrnn)

(defmethod add-dendrite! ((neuron neuron)(synapse synapse))
  "(Destructively) add a new dendrite to a neuron, and connect a synapse to it."
  (push synapse (neuron-dendrites neuron)))
