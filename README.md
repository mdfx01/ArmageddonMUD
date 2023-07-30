# ArmageddonMUD

## Arm Scripts
Follow the instructions https://tintin.mudhalla.net/install.php#iOS

Then, after that, return to the home directory
* cd ~

Then choose to download one of the following scripts

For a minimalist script which only gives you an infobar but does nothing else, good for those who want to do their own customizing:
* wget https://raw.githubusercontent.com/mdfx01/ArmageddonMUD/main/ArmMobile.tin

For a more full featured script which does infobar, logging, highlighting, mousewheel/inputbar clicking
* wget https://raw.githubusercontent.com/mdfx01/ArmageddonMUD/main/ArmBasic.tin

For all of the above plus mapping capabilities:
* COMING SOME DAY

Then run the script with:
* tt++ ArmMobile.tin

The commands are the same as the Mushclient infobar to make it work properly.  Run:
 - InstallInfobar
 - ManaOn  (will turn mana on in your prompt)
 - ManaOff (Default, will turn mana off in your prompt)

To quit out of tintin++, use #end

The script itself is very basic, there are only two highlights for stuff in the prompt.  Lots of customisation can be done.

## Shiny Skills Tool
This is an application for comparing skills across class/subclass combinations in a graphical format.  It's a bit rough and hacked together but works.  The two csv files also give the skill data in a format that may be useful to others.  It is missing "branches_from" if anybody wanted to fill that in.
