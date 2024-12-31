0.2.1
==================

Bug fixes:
* Fix bug whereby IPA literals with a feature alpha modifier were not correctly matching.
    * For example, `l:[Asyll] > r:[Asyll]` would not match the positive case.