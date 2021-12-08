LEANINK ?= ../build/bin/lake

build:
	./test/test.sh build_leanink

capture: 
	./test/test.sh run_capture

run_tests:
	./test/test.sh run_tests