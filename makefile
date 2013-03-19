all: minijs

minijs: syntax.scala values.scala domains.scala interpreter.scala gc.scala freelist.scala
	fsc -d build -unchecked -deprecation syntax.scala values.scala domains.scala interpreter.scala gc.scala freelist.scala

clean:
	rm -rf build/*