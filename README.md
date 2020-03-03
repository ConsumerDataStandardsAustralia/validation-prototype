# Consumer Data Standards - Validation Suite

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

- A test suite for verifying a data holder
- A test suite for verifying a data consumer
- A mock data consumer app for manually testing and developing a data provider
- A mock data provider/bank api for manually testing and developing a data consumer

Our plan will be to deliver these components in an MVP fashion, delivering things
in roughly this order:

- A bare bones mock consumer and producer so that people can start using these for
  developing their implementations.
- The basic validation suite testing one major auth flow.
- And then iterating them both until they are fully featured.

All software will be developed as open sourced (MIT) software components in this
repository.

These software components will be written in the Haskell programming language
as our team aims to leverage the power of property-based state machine testing
of the auth protocols. This method of testing is particularly effective at
finding edge cases in distributed systems. We feel that this should have a
large impact testing the FAPI/Open ID Connect/OAuth implementations, which
should help ensure that Australia’s open banking implementations are of the
highest possible security standards.

Mock Data Holder ("LambdaBank")
============================================================================
Along with the other artifacts, this repository also contains a script to build a docker image containing a mock Data Holder (DH). The script can be found in `consumer-data-au-lambdabank/scripts/dockerisation.sh`. This image allows for easy deployment into test environments without any Haskell experience necessary.

The mock DH implements all of the defined resource endpoints in the Banking API, responding with mock data that is conformant to the CDS draft standards (v0.2.0 - 20/12/2018). The latest version of the standards can be found at Consumer Data Standards Australia.

When the docker image is running, the mock DH server will be available on port 8000. The server accepts a limited range of requests, and will return well-formed dummy responses. A basic HTML client that makes requests to the mock DH can be found in [consumer-data-au-lambdabank/res/index.html](./consumer-data-au-lambdabank/res/index.html).

A prebuilt docker image is also available from Docker Hub in the `qfpl/cds-validation-prototype` [repository](https://cloud.docker.com/repository/docker/qfpl/cds-dh-prototype).
