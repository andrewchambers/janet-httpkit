(import uri)

(defn parse-www-form-urlencoded
  [d]
  "Parse a www-form-urlencoded query string returning a table or nil."
  (uri/parse-query 
    (string/join (string/split "+" d) "%20")))

(defn- is-ctl-char
  [s]
  (def c (s 0))
  (or (and (>= c 0) (<= c 31))
      (= c 127)))

(defn- is-cookie-octet
  [s]
  (def c (s 0))
  (or
    (= c 0x21)
    (and (>= c 0x23) (<= c 0x2b))
    (and (>= c 0x2d) (<= c 0x3a))
    (and (>= c 0x3c) (<= c 0x5b))
    (and (>= c 0x5d) (<= c 0x7e))))

# https://tools.ietf.org/html/rfc2616
# https://tools.ietf.org/html/rfc6265
(def- cookie-header-peg ~{
   :main (sequence :OWS :cookie-string :OWS (not 1))
   :OWS (any (sequence (opt :obs-fold) :WSP))
   :obs-fold "\r\n"
   :WSP (set " \t")
   :cookie-string (sequence :cookie-pair (any (sequence "; " :cookie-pair)))
   :cookie-pair (sequence :cookie-name "=" :cookie-value)
   :cookie-value (choice
                   (capture  (any :cookie-octet))
                   # XXX do we need to unescape within the quotes?
                   (sequence "\"" (capture (any :cookie-octet)) "\""))
   :cookie-name (capture :token)
   :cookie-octet (drop (cmt (capture 1) ,is-cookie-octet))
   :token (some (sequence (not (choice :CTL :separator)) 1))
   :CTL (drop (cmt (capture 1) ,is-ctl-char))
   :separator (set "()<>@,;:\\\"/[]?={} \t")
})

(defn parse-cookie-header
  [h]
  (when-let [m (peg/match (comptime (peg/compile cookie-header-peg)) h)]
    (table ;m)))

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