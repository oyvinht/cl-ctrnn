(defpackage #:continous-time-recurrent-neural-network
  (:use "COMMON-LISP")
  (:nicknames #:ctrnn)
  (:export
   #:*timestep*
   #:add-dendrite!
   #:add-neuron!
   #:firing-frequency
   #:motor-neuron
   #:motor-neuron-motor-function
   #:neural-network
   #:neural-network-neurons
   #:neuron
   #:neuron-bias
   #:neuron-dendrites
   #:neuron-external-current
   #:neuron-external-current-magnitude
   #:neuron-membrane-potential
   #:neuron-time-constant
   #:pacemaker
   #:pacemaker-io-neuron
   #:pacemaker-hidden-neuron
   #:sensor-neuron
   #:sensor-neuron-sensor-function
   #:synapse
   #:synapse-from-neuron
   #:synapse-weight
   #:synchronously-update-membrane-potentials!
   #:update-membrane-potential!))

(in-package :ctrnn)

(defparameter *timestep* 0.01)