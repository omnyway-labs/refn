
CODOX_DIR ?= api-docs

$(CODOX_DIR):
	mkdir -p $@

codox: $(CODOX_DIR)
	clojure \
    -A:dev \
	  -Srepro \
	  -Sdeps "{:deps {codox/codox {:mvn/version \"0.10.7\"}}}" \
	  -e "(require 'codox.main)" \
	  -e "(codox.main/generate-docs \
	        {:language :clojurescript \
           :metadata {:doc/format :markdown} \
           :output-path \"$(CODOX_DIR)\"})"
