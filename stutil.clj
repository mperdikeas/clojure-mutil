(ns stutil)


(import '(org.stringtemplate.v4 STGroupFile))

(defn template [filepath delimA delimB templateName]
  (.getInstanceOf
   (STGroupFile. filepath delimA delimB)
   (name templateName)))


(defn render [template bindings]
  (let [applyBindings
        (fn [template bindings]
            (doseq [ [k v] bindings] (.add template (name k) v)))
        ]
    (applyBindings template bindings)
    (.render template)))

(defmacro _render [template_file delimA delimB production bindings]
  `(let [
         tmplt# (template ~template_file ~delimA ~delimB (quote ~production))
         ]
     (render tmplt# ~bindings)))


(defmacro create-render-macro
  ([macroName pathToFile]
     `(create-render-macro ~macroName ~pathToFile \< \>))
  ([macroName pathToFile delimA delimB]
     `(defmacro ~macroName [production# bindings#]
        (let
            [TEMPLATE_FILE# ~pathToFile]
          `(_render ~TEMPLATE_FILE# ~~delimA ~~delimB ~production# ~bindings#)))))

(defmacro <_ [keyword] ;; TODO what's wrong with the following evaluation?
  `{~keyword `(eval (symbol (name ~keyword)))})
