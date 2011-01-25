# Finger Tree change log

### Changes from 0.0.1 to 0.0.2-SNAPSHOT

Breaking changes:
- Renamed `consl` to `conjl` (same argument order)
- Removed `conjr` -- just use `conj` for appending on the right

New:
- Implemented equality
  - counted-sorted-set can be equal to both sets and sequentials
- Implemented hashCode
- Implemented support for meta and with-meta (IObj)

### Version 0.0.1

This is essentially the version described in the [Clojure Conj talk][1]

[1]: http://talk-finger-tree.heroku.com/
