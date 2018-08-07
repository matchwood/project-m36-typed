# Project M36 Typed
This is an experimental implementation of a typed interface for [Project M36](https://github.com/agentm/project-m36). I am not planning to develop this much further for the time being, but would be happy to answer questions and provide details and support if anyone wants to continue development on any of the code here.

For basic usage please see the `test/Test.hs` file.

## Features

A type level representation of the project M36 language, allowing you to define a type safe database schema and issue queries on it.

Refinements of this include a way of representing foreign key constraints within your schema, and even auto-deriving them based on your types (see `InjectConstraints` in `ProjectM36.Typed.Schema`). 

Once your schema has been defined, this library provides a way to automatically create that schema within a project M36 database (`connectProjectM36T`) and from that point on gives your a type safe interface for interacting with your database with compile time guarantees that you can only insert and query types that are in your database, and within queries can only reference fields that your types actually have. 

It additionally provides some code for automatically wrapping types with some metadata (like createdDate, uniquely generated primary ids etc).