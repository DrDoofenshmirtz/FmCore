(ns
  #^{:doc "Share resources with mutable state between clients."
     :author "Frank Mosebach"}
  fm.core.shared-resource
  (:use [clojure.contrib.except :only (throw-if-not)]))

(defn- check-lifecycle-functions [create delete]
  (throw-if-not create (IllegalArgumentException. "Illegal create function!"))
  (throw-if-not delete (IllegalArgumentException. "Illegal delete function!")))

(defn shared-resource
  "Creates a shared resource with the given lifecycle functions and an optional
  predicate to be used for testing the equality of two resource instances. If
  none is given clojure.core/identical? will be used."
  ([create delete]
    (shared-resource create delete nil))
  ([create delete equals]
    (check-lifecycle-functions create delete)
    (let [shared-resource (atom create)
          equals (or equals identical?)
          lock (Object.)]
      (fn [resource-task]
        (locking lock
          (let [[result resource] (resource-task
                                    @shared-resource
                                    create
                                    delete
                                    equals)]
            (reset! shared-resource resource)
            result))))))

(defn acquire
  "Retrieves or creates a resource instance. A nil return value indicates that 
  the shared resource has been destroyed and not been reset to its initial
  state."
  [shared-resource]
  (shared-resource (fn [resource create _ _]
                     (let [resource (cond
                                      (nil? resource) nil
                                      (identical? create resource) (create)
                                      :else resource)]
                       [resource resource]))))

(defn discard
  "Discards the given resource instance using the shared resource's delete
  function. If the given resource equals the resource instance currently
  residing inside the shared resource the shared resource will be reset to
  its initial state."
  [shared-resource discarded]
  (let [destructor (shared-resource (fn [resource create delete equals]
                                      (if (and
                                            (not (nil? resource))
                                            (equals resource discarded))
                                        [#(delete resource) create]
                                        [#(delete discarded) resource])))]
    (destructor)))

(defn reset
  "Resets the shared resource to its initial state, calling the delete function
  on the resource instance currently residing inside the shared resource."
  [shared-resource]
  (if-let [destructor (shared-resource (fn [resource create delete _]
                                         (if-not (or
                                                   (nil? resource)
                                                   (identical? create resource))
                                           [#(delete resource) create]
                                           [nil create])))]
    (destructor)))

(defn destroy
  "Destroys the shared resource, calling the delete function on the resource
  instance currently residing inside the shared resource."
  [shared-resource]
  (if-let [destructor (shared-resource (fn [resource _ delete _]
                                         (if-not (nil? resource)
                                           [#(delete resource)])))]
    (destructor)))

(defn in-use?
  "Checks if in-use equals the resource instance that is currently residing 
  inside the shared resource."
  [shared-resource in-use]
  (shared-resource (fn [resource _ _ equals]
                     [(and
                        (not (nil? resource))
                        (equals resource in-use))
                      resource])))
