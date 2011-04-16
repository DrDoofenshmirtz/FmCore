(ns fm.core.function-parser)

(defstruct parse-state :char-seq :result)

(defstruct operation :function :precedence)

(def +operation-map+ {\+ (struct operation + 0)
                      \- (struct operation - 0)
                      \* (struct operation * 1)
                      \/ (struct operation / 1)})

(def +variable-names+ #{\x \y \z})

(defn min-precedence []
  (apply min (map :precedence (vals +operation-map+))))

(defn max-precedence []
  (apply max (map :precedence (vals +operation-map+))))

(defn illegal-expression [char-seq]
  (throw (IllegalArgumentException.
           (str "Illegal Expression: \"" (apply str char-seq) "\"!"))))

(defn skip-blanks [char-seq]
  (drop-while #(Character/isWhitespace %) char-seq))

(defn parse-next-if [pred char-seq]
  (let [next-char (first char-seq)]
    (if (pred next-char)
      (struct parse-state (rest char-seq) next-char)
      (struct parse-state char-seq nil))))

(defn left-bracket? [chr]
  (= \( chr))

(defn right-bracket? [chr]
  (= \) chr))

(defn sign-or-digit? [chr]
  (and chr (or (= chr \-) (Character/isDigit chr))))

(defn variable? [chr]
  (+variable-names+ chr))

(defn parse-sign-or-digit [char-seq]
  (parse-next-if sign-or-digit? (skip-blanks char-seq)))

(defn parse-left-bracket [char-seq]
  (parse-next-if left-bracket? (skip-blanks char-seq)))

(defn parse-right-bracket [char-seq]
  (parse-next-if right-bracket? (skip-blanks char-seq)))

(defn parse-number [char-seq]
  (let [{char-seq :char-seq sign-or-digit :result} (parse-sign-or-digit char-seq)]
    (if sign-or-digit
      (let [[digits char-seq] (split-with #(Character/isDigit %) char-seq)]
        (try 
          (struct parse-state char-seq (Integer. (apply str sign-or-digit digits)))
          (catch NumberFormatException _ (struct parse-state (cons sign-or-digit char-seq) nil))))
      (struct parse-state char-seq nil))))

(defn make-bind-variable-function [variable]
  (letfn [(bind-fn
            ([]
              (bind-fn {}))
            ([bindings]
              (or (bindings (keyword (str variable))) 0)))]
    bind-fn))

(defn parse-variable [char-seq]
  (let [{char-seq :char-seq variable :result} (parse-next-if variable? (skip-blanks char-seq))]
    (if variable
      (struct parse-state char-seq (make-bind-variable-function variable))
      (struct parse-state char-seq nil))))

(declare parse-expression)

(defn parse-operand [char-seq]
  (let [{char-seq :char-seq left-bracket :result} (parse-left-bracket char-seq)]
    (if left-bracket
      (let [{char-seq :char-seq result :result} (parse-expression char-seq)
            {char-seq :char-seq right-bracket :result} (parse-right-bracket char-seq)]
        (or right-bracket (illegal-expression char-seq))
        (struct parse-state char-seq result))
      (let [{char-seq :char-seq number :result} (parse-number char-seq)]
        (if number
          (struct parse-state char-seq (constantly number))
          (let [{char-seq :char-seq bind-fn :result} (parse-variable char-seq)]
            (struct parse-state char-seq bind-fn)))))))

(defn parse-operation [char-seq]
  (let [char-seq (skip-blanks char-seq)
        operation-sym (first char-seq)
        operation (+operation-map+ operation-sym)]
    (struct parse-state (if operation (rest char-seq) char-seq) operation)))

(defn make-evaluation-function [operation lhs-fn rhs-fn]
  (letfn [(eval-fn
            ([]
              (eval-fn {}))
            ([bindings]
              ((operation :function) (lhs-fn bindings) (rhs-fn bindings))))]
    eval-fn))

(defn parse-expression  
  ([char-seq]
    (let [{char-seq :char-seq lhs :result} (parse-operand char-seq)]
      (or lhs (illegal-expression char-seq))
      (parse-expression lhs char-seq)))
  ([lhs char-seq]
    (let [{char-seq :char-seq operation :result} (parse-operation char-seq)]
      (if-not operation
        (struct parse-state char-seq lhs)
        (let [{char-seq :char-seq rhs :result} (parse-operand char-seq)]
          (or rhs (illegal-expression char-seq))
          (parse-expression lhs operation rhs char-seq)))))
  ([lhs operation rhs char-seq]
    (if (= (operation :precedence) (max-precedence))
      (parse-expression (make-evaluation-function operation lhs rhs) char-seq)
      (let [{char-seq :char-seq next-operation :result} (parse-operation char-seq)]
        (if-not next-operation
          (parse-expression (make-evaluation-function operation lhs rhs) char-seq)
          (let [{char-seq :char-seq next-rhs :result} (parse-operand char-seq)]
            (or next-rhs (illegal-expression char-seq))
            (parse-expression lhs operation rhs next-operation next-rhs char-seq))))))
  ([lhs operation rhs next-operation next-rhs char-seq]
    (if (>= (operation :precedence) (next-operation :precedence))
      (parse-expression
        (make-evaluation-function
          next-operation
          (make-evaluation-function operation lhs rhs)
          next-rhs)
        char-seq)
      (parse-expression
        (make-evaluation-function
          operation
          lhs
          (make-evaluation-function next-operation rhs next-rhs))
        char-seq))))

(defn parse-function [char-seq]
  ((parse-expression char-seq) :result))
