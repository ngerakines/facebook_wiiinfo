# About #

facebook\_wiiinfo is an open source Facebook application written in Erlang. It has one feature: It allows you to quickly display a Wii code on the info tab of your profile.

# Why Erlang? #

The total time developing, testing and deploying this Facebook application was under 2 hours. Erlang has agile practices backed into the language allowing you to do neat things like live code hot-swaping. This application uses Mnesia as a data store layer (comes with the standard package) and Yaws as a web interface which are really simple to setup and use.

# Installation #

Installing this Facebook application for your own use is pretty simple. It does require the erlang\_facebook and rfc4627 modules, both of which can be found on GitHub. Once you've got a working copy of this repository all you need to do is make sure yaws, erlang\_facebook module and the rfc4627 module are in the Erlang lib path.

You'll also need to set your own Facebook application "API\_KEY" and "SECRET" on line 40 in wifbfe.erl file.

Once all that is taken care of all you need to do is run `make start`. By default the service is bound to port 6010 but that can be changed in the wifbfe\_yaws.erl file.

# Questions? Comments? Bugs? #

Feel free to email me at nick@gerakines.net or contact me through Facebook if you've got any questions or comments. Bug reports and patches are welcome.

# License #

Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>

This project has been open sourced under the MIT license.
