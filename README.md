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
* wget https://raw.githubusercontent.com/mdfx01/ArmageddonMUD/main/ArmWithMap.tin

Note for the mapping to work, you need to separately create a map call "MyMap.map" from within TinTin++.  The script loads this map.  Over time you will want to add to the triggers.

To create your own new blank map, launch tt++ then do:
```
#map create 200000
#map goto 1
#map flag vtmap on
#map write MyMap.map
```
Then load the script with mapping capabilities, which loads MyMap.map.  You can change the names of everything if you want.

You can run a script with a command like:
* tt++ Arm.tin

The commands are the same as the Mushclient infobar to make it work properly.  Run:
 - InstallInfobar
 - ManaOn  (will turn mana on in your prompt)
 - ManaOff (Default, will turn mana off in your prompt)

To quit out of tintin++, use #end

The script itself is very basic, there are only two highlights for stuff in the prompt.  Lots of customisation can be done.

## Shiny Skills Tool
This is an application for comparing skills across class/subclass combinations in a graphical format.  It's a bit rough and hacked together but works.  The two csv files also give the skill data in a format that may be useful to others.  It is missing "branches_from" if anybody wanted to fill that in.
