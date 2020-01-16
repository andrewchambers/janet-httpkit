(import ../httpkit :as httpkit)

(assert 
  (deep=
    (httpkit/parse-cookie-header "a=b; b=c")
    @{"a" "b" "b" "c"}))

