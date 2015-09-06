## Keynote: An Enterprise Software Consultant's view of FP
Amanda Laucher – Pariveda Solutions

No more rewriting CRUD every 5 years.
"The baseline cost for any software development is quarter million dollars” - Medium Sized Business Owner in Greene County, PA, USA
I can’t write software because I can’t afford it.
If we don’t have to rewrite software as much.  If we can write software more efficiently.
These means interesting projects and real innovation.
Code that tells me when my laundry is done is not changing the world.
Great pay for great work.
“Advocates of FP languages claim they produce an order of magnitude improvement in productivity, experiments don’t always verify that figure - sometimes they show an improvement of only a factor of four” - Philip Wadler.
An “Object head” in Erlang Land.
Whatapp -> 900 millions monthly active users.
"FP has to win - it is the only right answer - obviously"
"Why Big Data Needs to Be Functional" - deanwampler NE Scala Sumposium. (edited)
2012 - rails died. (edited)
No more hype - because there is no more reason for hype, FP is just the normal now.
“FP has won all of the battles"
“But is losing the war"
“Stuck in an echo chamber"
Farming - lots of cool stuff.  Big Data, Internet of Things, Scala, Cloud data, Streaming data, predictions. (edited)
“I’ve never been in the same room as someone who knew Scala"
“Telling the Functional Programming Story” Simon St. Laurent - Keynote and Erlang ….
Bestseller by Year.
Books aren’t that sellers in Technology.
Erlang, Lisp, Haskell, Scala, F#, etc. 2007-2012 (edited)
5/6 FP best sellers is Scala in 2015
Javascript The Good Parts 150 books/week.
All FP books combined 200 books/week
Javascript has become the new Ruby.
Reasons why Functional Program am not win:
0 Algebro, 1 Monads. 2 Jerks. 3. Jerks writing about monads
“Why no one uses functional languages” - Philip Wadler
Compatibility. Libraries. Portability and installation. Availability. Footprint. Tools. Training. Popularity. Performance. “They don’t get it."
All solved except “They don’t get it"
Alpha geeks are not necessarily the smartest people in the room.
They see a little bit of everything and bringing it back to the company.
Alpha geeks are leaving.
Meetups: Rails: 385,382 attendees.  All FP combined: 214,671.  Women in Technology: 172,337.
Mainstream conferences aren’t inviting FP speakers anymore.
FP: The failure of state.  Robert C Martin
See YouTube.
Garbage collection is a terrible hack that violates Functional Programming - Uncle Bob Martin (harmful)
"We don’t really need FP in micro services.  You can use anything since it’s just a few lines of code.” - Enterprise Software Engineer.
Because they’re deploying to the cloud.
Infrastructure is Code is important - but ...
Lambda.  Small code.  All managed for you.
Spinning up JVMs in the cloud to run 100 lines of code.
10s response time, but JVM is causing problems, not a way to the future. (edited)
"The increase is use of FP languages is slowing” - Software Consultant VP
Is FP losing popularity?
TOBLE? Accurate?
F# only FP on the list.
Don’t stop talking about it.
“Why would I use a JVM language that is FP when I can use Java 8?"
Technology adoption curve.
Tech fundamentalists -> Visionaries -> The chasm (you are here) -> Pragmatists -> Conservatives -> Skeptics. (edited)
“40-50 years of FP and we are still early adopters”
Always Be Closing
Influencing Executives.
More case studies - competitive advantage
“What are we teaching in HS, University, trade schools?"
Don’t compare to OO to an exec.
More talks, blogs, meetups.
Influencing Silent Experts
How to get rid of Intersubjectivity?  “Monad", “Parametricity", "Applicative Functor"
“I Love Math"
“She doesn’t know that she’s doing algebra"
We don’t teach math in a way that people want to learn it.
Theory of Cognitive Dissonance.  If you expect it, you will see it.
We can hire any smart person.  They will learn.
“Several people have told me that my inability to suffer fools gladly is one of my main weaknesses” - Edsger Dijkstra
“FP simplifies things by removing a lot of unnecessary complexity so you can focus on the task” New Notre Dame EE Graduate

## Coping with change: data schema migration in Haskell
Adam Gundry – Well-Typed LLP

Types keep us honest.
Serialisation and types.  Problem, how to load old data with new types?

Model DSL to describe the data schema:
* Smple: basic types, records, unions, enums
* Template Haskell to generate Haskell datatypes
* Code generation (eg. conversion to JSON)
* Documentation generation (edited)
NEW MESSAGES

Change Log DSL to describe schema changes:
* Human and machine readable
* Automatically checked by CI server
* Data automatically migrated on deployment

```data User = record { name :: Username, admin :: Bool }

data Username = String

version “1.0"
```

```data User = record { name :: Username, admin :: Bool, logins: LoginCount }

data Username = String

version “0.3”
  changed record User
    field removed admin
version “0.2"
  changed record User
    field added
      logins :: LoginCount
      default 0
  added LogingCount
    basic integer
version “1.0"
```
(edited)

What this gives us:
* Validation:
** Correctness: are all changes meaningful?
** Completeness: Can we get from old versions to the latest?
** Run locally before commit, or by CI server.
* Migration
** Automatically convert old data into new format
** Run in automated deployment scripts and tests (edited)

```Schema changelog validation failed:

Changelog incomplete: Differences between log version (0.2) and latest version (0.3):

changed record User
  field removed admin
```

The migration process:
* Export old application’s dataset as JSON
** (includes version number and schema)
* Install new version of application
* Find the version number in the changelog
* Apply changes to schema and data in parallel
* Check resulting schema is as expected
* Import JSON dataset into new application

Example migration:
```
{“username": “Adam”, logins: () }
{“username": “Adam”, “admin”: true , logins: ()}
{“username": “Adam”, “admin”: true }
version “0.3”
 changed record User
   field removed admin
version “0.2"
 changed record User
   field added
     logins :: LoginCount
     default 0
 added LogingCount
   basic integer
version “1.0" (edited)
```

Custom migrations can run arbitrary Haskell code.

What works?
* Easy to make routine changes
* Validation and testing catch most errors early
* Error messages can (usually) be copy-pasted into change log
* Refactoring the schema becomes feasible
* Main change log has approximately 250 versions, 2000 changes

Potential pitfalls
* Forced linearity of change log
* Potentital for bugs in custom migration code
* Downtime due to offline migrations

The future
* CBOR (Concise Binary Object Representation)
* Bidirectional migrations
* Stronger typing guarantees for generic representation.

"iris connect"


## Haskell in production, a survey
Christian Marie – Anchor Systems

“There is only one way: Go within. Search for the cause, find the impetus that bids you write. Put it to this test: Does it stretch out its roots in the deepest place of your heart? Can you avow that you would die if you were forbidden to write? Above all, in the most silent hour of your night, ask yourself this: Must I write? Dig deep into yourself for a true answer. And if it should ring its assent, if you can confidently meet this serious question with a simple, “I must,” then build your life upon it. It has become your necessity. Your life, in even the most mundane and least significant hour, must become a sign, a testimony to this urge.”
― Rainer Maria Rilke, Letters to a Young Poet

`sensei` a ghci based persistent test runner.

`tinc` caches cabal installs

Board -> code -> review -> merge -> staging -> test -> production

## Fighting spam with Haskell at Facebook
Simon Marlow – Facebook

`Sigma :: Content -> Bool`
`cufpSpammer :: Haxl Bool`
Haxl is a Monad
`.&&` is `&&` lifted into Haxl
`ApplicativeDo` in GHC 8.0 (y)
Most difficult parts that weren’t expected: Functionality that wasn’t obvious in the start.
End-To-End
Pleasantly surprised with how few performance problems there were
One unrepresentative benchmark  was 10k times faster. (edited)
Linux perf tool.  See Simon Marlow’s blog.
Compatibility layer that includes deprecation and warnings for translated code.
Can’t check-in code that crashes the system.
But can still cause problems for other systems.
Unloading and allocation limits in 7.10.
C++ mangling code may be open-sourced.
Applicative `do` notation vs `fmap`, surprise that some people prefer the later.  Also people happy to use `sequence` and `mappend` (edited)

## Modeling state transitions with specification-based random testing
Zeeshan Lakhani – Basho

“Don’t write tests: Generate them.” <-- mantra
Modelling ChaosMonkey in QuickCheck.
Modelling processes in Erlang.
