# R/state.R
.perinary_internal <- new.env(parent = emptyenv())
.perinary_internal$dictionary <- NULL

# Tracks which (dict_version -> current_version) pairs have already triggered
# a version-mismatch warning this session so the warning fires at most once
# per unique pairing.
.perinary_internal$version_warned <- character(0)
