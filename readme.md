## Goal

* Able to model OpenApi v3 as AST in Scala
* Able to convert AST to http4s code (code gen)

## Implementation plan

Milestone 1: Able to convert `petstore.yml` to model classes
Milestone 2: Able to convert `petstore.yml` to Http4s routes
Milestone 3: Able to convert `petstore.yml` to Sttp Client code

## How to work with this project?

This project contains 3 parts:

1. core - contains code to convert an OpenApi spec file into scala AST
2. sbt-apigen - wrap `core` into a sbt plugin, exposes some settings
3. example - a test project to test `sbt-apigen`

To develop, this is what I do:

1. Make change in core/sbt-apigen
2. compile and run `publishLocal` from root project, this will publish SNAPSHOT version of `sbt-apigen`
3. Start sbt session in `example` project, and compile, then you can find generated code in [example/target/scala-2.12/src_managed/codegen/generated.scala](example/target/scala-2.12/src_managed/codegen/generated.scala)

For some reason, Intellij does not understand the generated code in `example` project

### Things I've learned

1. scala meta is awesome, thanks Olaf!
2. when you feel you need some sort of control inversion, eg. passing context through multiple layers to control a behaviour down the call chain, consider model your call-chain as function where the param is 1st class citizen, then each layer can now read context from caller, and decide context for callee, in a principled way
3.    

## Todo

* use iota's coproduct instead of sealed trait (sealed trait is not open and does not faithfully represent the idea in open api spec)
