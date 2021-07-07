(ns merkdo-backend.db.schema
  (:require
    [datomic.api :as d]
    [datomic.client.api :as dc]
    [hodur-engine.core :as hodur]
    [hodur-datomic-schema.core :as hodur-datomic]
    [merkdo-backend.db.spec-schema :as hodur-spec]
    [hodur-graphviz-schema.core :as hodur-graphviz]
    [datomic-spec.core :as ds]                              ; ostavi
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [integrant.core :as ig]))


(def datomic-mkdo-schema
  '[^{:datomic/tag true
      :lacinia/tag true
      :graphviz/tag true}
    default

    Account
    [^{:type String
       :datomic/unique :db.unique/identity}  name
     ^{:type Account-type} type
     ^{:type Brand-group :optional true} brand-group
     ^{:type Country :optional true} country]

    Country
    [^{:type String
       :datomic/unique :db.unique/identity } code
     ^{:type String }   name]

    Currency
    [^{:type String
       :datomic/unique :db.unique/identity } code
     ^{:type Boolean}   virtual
     ^{:type Integer :optional true} specificity
     ^{:type Brand-group} brand-group
     ^{:type Country :optional true} country
     ^{:type Account} account]

    Brand-group
    [^{:type String
       :datomic/unique :db.unique/identity}   name
     ^{:type String }   description]

    Exchange-rate
    [^{:type Currency :optional true} from
     ^{:type Currency :optional true} to
     ^{:datomic/type :db.type/double} rate
     ^{:type Boolean}  active
     ^{:type Integer :optional true} at]

    Transfer
    [^{:type Account-type
       :cardinality [2]} direction
     ^{:datomic/type :db.type/double} value
     ^{:datomic/type :db.type/double} rate
     ^{:type Currency} to-currency
     ^{:type Currency} from-currency
     ^{:type Account} to-account
     ^{:type Account} from-account
     ^{:type Integer} at]

    Conto
    [^{:type String
       :datomic/unique :db.unique/identity} identifier
     ^{:type Account} account
     ^{:type Account :optional true} from-account
     ^{:type Currency} currency
     ^{:type Currency :optional true} from-currency
     ^{:datomic/type :db.type/double} balance
     ^{:datomic/type :db.type/double} min-balance
     ^{:datomic/type :db.type/double :optional true} exchange-rate
     ^{:datomic/type :db.type/keyword
         :cardinality [0 n] :optional true} tags
     ^{:type Integer :optional true} at]

    Transaction
    [
     ^{:type Transaction-type
       :cardinality [2]} type
     ^{:datomic/type :db.type/double} value
     ^{:type Account} account
     ^{:type Conto} conto
     ^{:type Currency} currency
     ^{:type Transfer} transfer
     ^{:datomic/type :db.type/double :optional true} exchange-rate-difference
     ^{:type Integer :optional true} at]


    ^{:enum true}
    Transaction-type
    [credit debit]

    ^{:enum true}
    Account-type
    [merkdo retailer brand sender receiver]])

(s/def :non-blank-string? (s/and string? (complement str/blank?)))

(s/def :pos-float (s/and float?  pos?))

(def spec-mkdo-schema
  '[^{:spec/tag true
      :graphviz/tag true}
    default

    Db
    [^{:type Integer} id]

    Account
    [^{:spec/override :non-blank-string?}
      name
     ^{:type Account-type}
      type
     ^{:type Db :optional true}
      brand-group
     ^{:type Db :optional true}
      country]

    Country
    [^{:spec/override :non-blank-string?}
      code
     ^{:spec/override :non-blank-string?}
      name]

    Currency
    [^{:spec/override :non-blank-string?}
      code
     ^{:type Boolean}
      virtual
     ^{:type Integer}
      specificity
     ^{:type Db :optional true}
      brand-group
     ^{:type Db}
      country
     ^{:type Db :optional true}
      account]

    Brand-group
    [^{:spec/override :non-blank-string?}
      name]

    Exchange-rate
    [^{:type Db :optional true}
      from
     ^{:type Db :optional true}
      to
     ^{:spec/override :pos-float}
      rate
     ^{:type Boolean}
      active
     ^{:spec/override pos-int? :optional true}
      at]

    Transfer
    [^{:type Account-type
       :cardinality 2}
     direction
     ^{:type Float}
      value
     ^{:type Float :optional true}
      rate
     ^{:type Db}
      to-currency
     ^{:type Db}
      from-currency
     ^{:type Db}
      to-account
     ^{:type Db}
      from-account
     ^{:spec/override pos-int?}
      at]

    Conto
    [^{:spec/override :non-blank-string?}
      identifier
     ^{:type Db}
      account
     ^{:type Db :optional true}
      from-account
     ^{:type Db}
      currency
     ^{:type Db :optional true}
      from-currency
     ^{:type Float}
      balance
     ^{:type Float}
      min-balance
     ^{:type Float :optional true}
      exchange-rate
     ^{:type Tags
       :optional true
       :cardinality [0 n]}
      tags
     ^{:spec/override pos-int? :optional true}
      at]

    Transaction
    [
     ^{:type Transaction-type}
      type
     ^{:spec/override :pos-float}
      value
     ^{:type Db}
      account
     ^{:type Db :optional true}
      conto
     ^{:type Db}
      currency
     ^{:type Db}
      transfer
     ^{:type Float :optional true}
      exchange-rate-difference
     ^{:type Db :optional true}
      temp-conto-id
     ^{:type Db :optional true}
      temp-conto
     ^{:spec/override pos-int? :optional true}
      at]

    Temp-conto
    [^{:type Db}
      account
     ^{:type Db}
      currency
     ^{:type Db :optional true}
      from-currency
     ^{:type Db :optional true}
      exchange-rate
     ^{:optional true :type Float}
      min-balance
     ^{:spec/override pos-int? :optional true}
      at]

    ^{:enum true}
    Tags
    [comission exchange-rate-diff discount-brand]

    ^{:enum true}
    Transaction-type
    [credit debit]
    ;
    ^{:enum true}
    Account-type
    [merkdo retailer brand sender receiver]])

(def init-schema (hodur/init-schema datomic-mkdo-schema))

(def datomic-schema
   (hodur-datomic/schema init-schema))



(hodur-spec/defspecs (hodur/init-schema spec-mkdo-schema) {:prefix :merkdo})

(s/def :datomic/temp-id
  (s/spec :datomic-spec.core/tempid
          :gen #(gen/return (d/tempid :db.part/user))))

(s/def :db/id (s/or :db/id integer? :tempid :datomic/temp-id))

(s/def :merkdo/db (s/or :id (s/keys :req [:db/id] :opt []) :dbid :db/id))

(s/def :datomic/transaction (s/spec (s/keys :req-un [:ds/db-before :ds/db-after :ds/tx-data :ds/tempids])))



(comment
  (def schema-spec (hodur-spec/schema (hodur/init-schema spec-mkdo-schema) {:prefix :merkdo}))

  (spit "diagram.dot" (-> (hodur-graphviz/schema (hodur/init-schema datomic-mkdo-schema))
                          (clojure.string/replace #"Brand-group" "BrandGroup")
                          (clojure.string/replace #"Account-type" "AccountType")
                          (clojure.string/replace #"Exchange-rate" "ExchangeRate")
                          (clojure.string/replace #"Transaction-type" "TransactionType")))

  (do
    (hodur-spec/defspecs (hodur/init-schema spec-mkdo-schema) {:prefix :merkdo})

    (s/def :db/id (s/or :db/id integer? :tempid :datomic/temp-id))
    (s/def :merkdo/db (s/or :id (s/keys :req [:db/id] :opt []) :dbid :db/id)))


  (doseq [spec schema-spec]
    (eval spec)))





