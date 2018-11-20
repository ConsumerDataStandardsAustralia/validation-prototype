# Consumer Data Standards - Conformance Suite

The Queensland Functional Programming Lab (QFPL) have been enlisted to assist
with the development of software related to open banking. This document
outlines what QFPL hope to achieve for open banking participants, as well as a
high level plan for how we intend to achieve those goals. QFPL are sharing
this publicly so that feedback can be solicited to ensure that their work has
the intended benefits. It is also hoped that by publicly stating our goals,
open banking participants can include access to QFPL’s tools in their
planning.

We intend to follow a process similar to the specification proposals, where we
publish our thoughts as github issues to solicit feedback and make sure we
aligned with public interest as we go.

Our main deliverable is a rigourous test suite for both data consumers and
providers to test their compliance with the spec. This will be in the form of
an automated suite and minimal set of mock components for manual verification,
and as a sandbox to develop against. These components will be:

- A test suite for verifying a data provider
- A test suite for verifying a data consumer
- A mock data consumer app for manually testing and developing a data provider
- A mock data provider/bank api for manually testing and developing a data consumer

Our plan will be to deliver these components in an MVP fashion, delivering things
roughly this order:

- A bare bones mock consumer and producer so that people can start using these for
  developing their implementations.
- The basic conformance suite testing one major auth flow.
- And then iterating them both until their are fully featured.

All software will be developed as open sourced (MIT) software components in this
repository.

These software components will be written in the Haskell programming language
as our team aims to leverage the power of property-based state machine testing
of the auth protocols. This method of testing is particularly effective at
finding edge cases in distributed systems. We feel that this should have a
large impact testing the FAPI/Open ID Connect/OAuth implementations, which
should help ensure that Australia’s open banking implementations are of the
highest possible security standards.

This software will be distributed as docker containers with reference docker
compose files to ensure an easy deployment and operation in test environments
without any Haskell experience. Implementors are free to use these components
as a guide (and the QFPL will offer support for this) but please also expect
non-Haskell reference implementations created by other teams closer to the
final draft release.
