ft
=====

![Build status]
(https://github.com/Elzor/ft/workflows/Erlang%20CI/badge.svg)

Test OTP application

*Production Config:*

    ./config/sys.config (after changing run `make rel`)


Test
-----

    $ make test

Release
-----

    $ make rel

Run release
-----

    $ ./_build/prod/rel/ft/bin/ft console
