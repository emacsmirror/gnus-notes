
EMACS := /usr/local/bin/emacs -Q --batch -L .
# helm
HLMC_LIB := /home/dias/.emacs.d/elpa/helm-core-20200413.823
HELM_LIB := /home/dias/.emacs.d/elpa/helm-20200421.1138
BBDB_LIB := /home/dias/.emacs.d/elpa/bbdb-20200102.403
ASYNC_LIB := /home/dias/.emacs.d/elpa/async-20200113.1745
# hydra
HYDRA_LIB := /home/dias/.emacs.d/elpa/hydra-20200306.913
LV_LIB := /home/dias/.emacs.d/elpa/lv-20200227.1301
# core
S_LIB := /home/dias/.emacs.d/elpa/s-20180406.808
# Package lint
PKG_LINT := /home/dias/.emacs.d/elpa/package-lint-20200419.406/package-lint.el
PKG_LIB := /home/dias/.emacs.d/elpa/package-lint-20200419.406 

ALL_LIBS := -L $(HELM_LIB) -L $(BBDB_LIB) -L $(ASYNC_LIB) \
	    -L $(HYDRA_LIB) -L  $(LV_LIB) -L $(S_LIB) -L $(HLMC_LIB) -L $(PKG_LIB)

EMACSALL := $(EMACS) $(ALL_LIBS)

.SUFFIXES: .el .elc			      
.PHONY: clean all lint-core

clean:
	rm -f *.elc

all: core helm org

core: gnus-mylist.elc

helm: gnus-mylist-helm.elc

org: gnus-mylist-org.elc

gnus-mylist.elc: gnus-mylist.el
	$(EMACS) -L $(HELM_LIB) \
		 -L $(BBDB_LIB) \
		 -f batch-byte-compile $<

gnus-mylist-helm.elc: gnus-mylist-helm.el
	$(EMACS) $(ALL_LIBS) -f batch-byte-compile $<

gnus-mylist-org.elc: gnus-mylist-org.el
	$(EMACS) $(ALL_LIBS) -f batch-byte-compile $<

lint-core: gnus-mylist.el
	$(EMACSALL) -l package-lint.el -f package-lint-batch-and-exit $<

