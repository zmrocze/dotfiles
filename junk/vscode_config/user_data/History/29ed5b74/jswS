.PHONY: all compile clean test mod_student
BUILD_PATH_ZI=./_build/install/default/bin/zi
BUILD_PATH_MOD_UWR=./_build/install/default/lib/mod_uwr/mod_uwr.cma
BUILD_PATH_MOD_STUDENT=./_build/install/default/lib/mod_student/mod_student.cma
all: compile

compile:
	dune build

install: all
	rm -f ./zi
	rm -rf mods
	mkdir mods
	if [ -e ${BUILD_PATH_MOD_STUDENT} ]; then (cd mods; ln -s ../${BUILD_PATH_MOD_STUDENT} .); fi
	if [ -e ${BUILD_PATH_MOD_UWR} ]; then (cd zisdk; rm -f mod_uwr.cma; ln -s ../${BUILD_PATH_MOD_UWR} .); fi
	if [ -e ${BUILD_PATH_MOD_STUDENT} ]; then (cd mods; rm -f mod_student.cma; ln -s ../${BUILD_PATH_MOD_STUDENT} .); fi
	ln -s ${BUILD_PATH_ZI} ./zi

mod_student : all
	mkdir -p mods
	cd mods; if [ ! -e mod_student.cma ]; then (ln -s ../${BUILD_PATH_MOD_STUDENT} .); fi

test: all mod_student
	python3 ./tools/tester.py --plugin mods/mod_student.cma # --registers-description=simple_caller

test_without_plugin: all
	python3 ./tools/tester.py # --registers-description=simple_caller

clean:
	rm -f ./zi
	dune clean

