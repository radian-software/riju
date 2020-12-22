# Riju

For now, this README just has some miscellaneous notes about the build
architecture that I'm planning to set up. Later, it will be converted
back into a proper README.

Steps to build Riju from scratch locally:

* Build the packaging Docker image.
* Generate a Debian package for each language.
* Build the runtime Docker image.
* For each language, install its Debian package into a fresh copy of
  the runtime Docker image and run its tests.
* Install every language's Debian package into a single copy of the
  runtime Docker image and run all the tests.

Build artifacts:

* Packaging image
* Runtime image
* Debian packages
* Application image

Steps to build Riju from cache locally:

* *To run:* Pull application image.
* *To build application image:* Pull runtime image and all Debian
  packages.
* *To build Debian packages:* Pull packaging image.
* *To build runtime image:* Build from scratch.
* *To build packaging image:* Build from scratch.

To manipulate published artifacts we basically want to do atomic
updates which keep the integration tests passing. Possible operations:

* *Rebuild packaging image:* This can be done at any time.
* *Rebuild runtime image:* Rebuild application image and verify
  integration tests are still passing. Do not rebuild any Debian
  packages.
* *Rebuild Debian package:* Verify unit tests are passing. Rebuild
  application image and verify integration tests are still passing. If
  rebuilding multiple Debian packages, then we can run the integration
  tests only once. If rebuilding enough Debian packages, the
  probability that at least one will fail is very high. We can then
  trigger a more targeted update. This process could be automated.
* *CI:* Rebuild packaging and runtime images if needed. Rebuild Debian
  packages for any changed languages. Fetch everything unchanged from
  registry. Rebuild application image and verify integration tests are
  still passing. If yes and operating on main branch, publish all
  artifacts and deploy.
