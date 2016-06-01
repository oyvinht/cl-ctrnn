;;;; Neurons for the CTRNN plus their methods
;;;;----------------------------------------------------------------------------
(in-package :ctrnn)


;;;;-----------------
;;;; Neuron classes -
;;;;-----------------
(defclass neuron ()
  ((bias
    :accessor neuron-bias
    :documentation ""
    :initarg :bias
    :initform 0.0)
   (dendrites
    :accessor neuron-dendrites
    :documentation "Contains a pointer to a synapse object."
    :initarg :dendrites
    :initform nil)
   (external-current
    :accessor neuron-external-current
    :documentation "External current, e.g. sensory input."
    :initarg :external-current
    :initform 0.0)
   (external-current-magnitude
    :accessor neuron-external-current-magnitude
    :documentation "The value to set the ext.-cur. to for f.ex. sensor neurons."
    :initarg :external-current-magnitude
    :initform 20.0)
   (maximum-membrane-potential
    :accessor neuron-maximum-membrane-potential
    :documentation "An upper limit of how much the cell membrane can store."
    :initarg :maximum-membrane-potential
    :initform 1000) ; NOTE: High value?
   (membrane-potential
    :accessor neuron-membrane-potential
    :documentation "The neurons membrane potential as it was last calculated."
    :initarg :membrane-potential
    :initform 0.0)
   (snapshot-firing-frequency
    :accessor neuron-snapshot-firing-frequency
    :documentation "A place to store the firing frequency at a given time."
    :initarg :snapshot-firing-frequency
    :initform 0.0)
   (time-constant
    :accessor neuron-time-constant
    :documentation "(> 0) How responsive the neuron is ('time constant')."
    :initarg :time-constant
    :initform 1.0))
  (:documentation "Parent of all neuron types."))

(defclass motor-neuron (neuron)
  ((motor-function
    :accessor motor-neuron-motor-function
    :documentation "A one arg. function mapping firing freq. -> motor force."
    :initarg :motor-function
    :initform #'(lambda (self) (declare (ignore self)))))
  (:documentation "A neuron mapping it's firing frequency into motor force."))

(defclass sensor-neuron (neuron)
  ((sensor-function
    :accessor sensor-neuron-sensor-function
    :documentation "A one-arg. func. setting the ext. current of the neuron."
    :initarg :sensor-function
    :initform #'(lambda (self) (declare (ignore self)))))
  (:documentation ""))


;;;;-----------------------
;;;; Activation functions -
;;;;-----------------------
(defmethod firing-frequency ((neuron neuron))
  "Return firing frequency based on membrane potential."
  (/ 1 (+ 1 (exp (- (+ (neuron-membrane-potential neuron)
                       (neuron-bias neuron)))))))

(defmethod update-membrane-potential! :around ((neuron motor-neuron))
  "Call the associated motor function. Return energy consumed."
  (call-next-method) ; Update membrane potential first
  (funcall (motor-neuron-motor-function neuron) neuron))

(defmethod update-membrane-potential! ((neuron neuron))
  "Calculate and modify neuron current membrane potential. Return energy used."
  (unless (> (neuron-membrane-potential neuron) ; Restrain from being enormous..
             (neuron-maximum-membrane-potential neuron)) ; may cause overflow
    (incf
     (neuron-membrane-potential neuron)
     (* *timestep* ; The integration timestep
        (* (/ 1 (neuron-time-constant neuron)) ; 1/tau (time constant).
           (+ (- (neuron-membrane-potential neuron)) ; Neg. of the curr. pot.
              (neuron-external-current neuron) ; Ext. curr.
              (loop for synapse in (neuron-dendrites neuron) ; Synaptic curr.
                  sum (* (synapse-strength synapse)
                         (neuron-snapshot-firing-frequency
                          (synapse-from-neuron synapse)))))))))
  0) ; TODO: Energy used

(defmethod update-membrane-potential! :before ((neuron sensor-neuron))
  (funcall (sensor-neuron-sensor-function neuron) neuron))
