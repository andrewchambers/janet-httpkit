(import uri)

(defn parse-www-form-urlencoded
  [d]
  "Parse a www-form-urlencoded query string returning a table or nil."
  (uri/parse-query 
    (string/join (string/split "+" d) "%20")))

# https://tools.ietf.org/html/rfc2616
# https://tools.ietf.org/html/rfc6265
(def- cookie-header-peg ~{
   :main (sequence :OWS :cookie-string :OWS (not 1))
   :OWS (any (sequence (opt :obs-fold) :WSP))
   :obs-fold "\r\n"
   :WSP (set " \t")
   :cookie-string (sequence :cookie-pair (any (sequence "; " :cookie-pair)))
   :cookie-pair   (sequence :cookie-name "=" :cookie-value)
   :cookie-value  (choice
                    (capture  (any :cookie-octet))
                      # XXX do we need to unescape within the quotes?
                      (sequence "\"" (capture (any :cookie-octet)) "\""))
   :cookie-name   (capture :token)
   :cookie-octet  (choice "\x21"
                    (range "\x23\x2b")
                    (range "\x2d\x3a")
                    (range "\x3c\x5b")
                    (range "\x5d\x7e"))
   :token (some (sequence (not (choice :CTL :separator)) 1))
   :CTL (choice 127 (range "\x00\x1f"))
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

(defn redirect
  [url]
  @{:status 302
    :headers @{"Location" url}})
