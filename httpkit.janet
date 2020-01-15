(import uri)

(defn parse-www-form-urlencoded
  [d]
  "Parse a www-form-urlencoded query string returning a table or nil."
  (uri/parse-query 
    (string/join (string/split "+" d) "%20")))


# inspired by https://golang.org/src/net/http/cookie.go
# XXX, escape values.
# XXX, date table for expires.
(defn cookie-string
  [name value &keys {
    :path path
    :domain domain
    :expires expires
    :max-age max-age
    :http-only http-only
    :secure secure
    :same-site same-site
  }]

  (string
    name 
    "="
    value
    (if path
      (string "; Path=" path)
      "")
    (if domain
      (string "; Domain=" domain)
      "")
    (if expires
      (cond
        (string? expires) (string "; Expires=" expires)
        (error "unimplemented date formatting"))
      "")
    (if max-age
      (string "; MaxAge=" max-age)
      "")
    (if http-only
      "; HttpOnly"
      "")
    (if secure
      "; Secure"
      "")
    (case same-site
      :none   "; SameSite=None"
      :lax    "; SameSite=Lax"
      :strict "; SameSite=Strict"
      "; SameSite")))