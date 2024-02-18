##
# Oooapi
#
# @file
# @version 0.1

OPENAPI_ATD := openapi_spec/openapi.atd
OPENAPI_FILE := openapi_spec/openapi_j

.PHONY: all
all: $(OPENAPI_FILE)_j.ml $(OPENAPI_FILE)_j.mli $(OPENAPI_FILE)_t.ml $(OPENAPI_FILE)_t.mli $(OPENAPI_FILE)_v.ml $(OPENAPI_FILE)_v.mli

$(OPENAPI_FILE)_j.ml $(OPENAPI_FILE)_j.mli: $(OPENAPI_ATD)
	atdgen -j -j-std $<

$(OPENAPI_FILE)_t.ml $(OPENAPI_FILE)_t.mli: $(OPENAPI_ATD)
	atdgen -t $<

$(OPENAPI_FILE)_v.ml $(OPENAPI_FILE)_v.mli: $(OPENAPI_ATD)
	atdgen -v $<

# end
