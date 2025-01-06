# Lisp Roguelike

This documentation is not great.

## utils.lisp

Contains general-purpose functions, many of which deal with simple vector math or custom printing.

## bsp-dungeon.lisp

Contains functions for generating a dungeon using binary space partitioning algorithms.

## colors.lisp

Contains functions and variables for dealing with color escape codes.

## class-definitions.lisp

This file contains the definitions for all classes used in the game.

### display-object

A base class for anything that can be displayed as a character.

**display-char** the character to display. Defaults to `#\#` for no particular reason.

**temp-char** if set to something other than `#\esc`, this character will be displayed by the `get-ascii` function instead of `display-char`.
Defaults to `#\esc`.

**name** the name of the object. Defaults to an empty string.

**color** the color of the display character, where applicable. Defaults to white.

### destructible-object

A base class for anything that can be damaged and destroyed.

**resist**, **immune**, and **vulnerable** impact how much damage the object takes when attacked. Default to nil.

**health** the amount of damage the object can take before being destroyed. Defaults to 1.

### stat-object

A base class for anything with stats. Inherits from `destructible-object`.

**def**, **str**, **dex**, **cha**, **con**, **det**, **per** all default to 0.

**intl** defaults to 0. the initarg is `:int`.

### equipment

A class for items and equipment that the player can find in the dungeon. Equipment should not be instantiated directly; instead use the `defequipment` macro.

Inherits `display-object` and `stat-object`.

**equip-slot** tells the game what slot to equip the item in. The value should be a symbol. Defaults to `'hand`.

**weaponp** determines whether or not to apply stat bonuses when the item is equipped in the hand slot. Defaults to nil.

**atk** the amount and type of damage done when the equipment is wielded in the hand slot. Attacks are lists where up to the first three values are numbers, then any number of damage types, then the `:status` key and any number of status objects.
Examples: `(1 4 slashing)` is 1d4 slashing damage, `(1 bludgeoning)` is one bludgeoning damage, and `(2 4 1 piercing :status #<STATUS {10028D6B63}>)` is 2d4+1 piercing damage with a status effect. Defaults to `(1 bludgeoning)`.

**breakable** does it break when thrown? Defaults to nil.

**throw-distance** the distance the item can be thrown. Defaults to 2.

**burn-time** the length of time the item burns for. Defaults to 0. Items with a zero burn time cannot be burnt.

**consumable** if t, the item is destroyed when used. Defaults to nil.

**container** should be used to keep track of any item "containing" this one. See `bottle` methods
in game.lisp for examples.

**fake-name** used instead of `name` if the identity of the item is secret.

**secretp** keeps the name of each instance secret until identified. Defaults to nil.

**identifiedp** keeps the name of the class secret until identified. Defaults to t.

**price** how much the item can be bought for in a shop. Defaults to 2.

### actor

A class similar to the Sprite of 2d graphics. It keeps track of an object at a position.
Actors should not be instantiated directly; instead, use the `defactor` macro.

Inherits `destructible-object` and `display-object`.

**pos** the position of the actor. A single cons cell, such as `(0 . 5)`.

**dynamicp** if t, the `update` function is called on the actor every action cycle. Defaults to nil.

**solid** if t, the actor cannot be moved through. Defaults to t.

**interact-action-only** if t, the actor can only be interacted with through the "interact" action. Defaults to nil.

**persistent-visibility-p** if t, the actor remains visible even when out of the player's line of sight. Defaults to nil.

**wallp** if t, the actor impacts how walls around it are displayed. Secret doors have it set to t. Defaults to nil.

**hiddenp** if t, the actor is not displayed. Defaults to nil.

**destructiblep** if t, the actor can be damaged and destroyed. Defaults to t.

**consumable** if t, the actor is destroyed when interacted with. Defaults to nil.

### combat-entity

Inherits `actor` and `stat-object`.
Should not be instantiated directly.

**equips** a hash table containing items equipped by the actor.

### player

Inherits `combat-entity`.
A player is automatically created and stored in the `*player*` variable.

**heal-clock** determines how much time is left before player regains a hit point. Defaults to 10.

**starvingp** if t, the player does not regain hit points from `heal-clock`. Defaults to nil.

**hunger** how long is left before the player starts starving. Defaults to 80.

**xp** how much XP the player has.

**max-health** max health.

**xp-bound** how much XP the player needs to level up.

### enemy

Inherits `combat-entity`.
`dynamicp` is set to t by default.
Should not be instantiated directly; instead use the `defenemy` macro.

**spd** how often the enemy activates compared to the player. A speed of 1 is exactly as fast as the player, and a speed of 2 is half as fast as the player. Defaults to 1.2.

**atk** an attack list as described in the equipment section. Can be a list of attack lists for multiple attacks per activation.

**xp** how much XP the enemy is worth when killed. Defaults to 1.

**loot** stuff the enemy drops when killed.

**enabled** if t, the enemy activates. If nil, the enemy does not activate. Defaults to nil.

### layer

A class for keeping track of each dungeon level.

**board** a hash table of board positions. The keys are positions, the values are either `'hidden` or `'found`.

**dynamic-actors** a list of actors on the layer that need to be updated.

**actors** a list of the rest of the actors.

**up-ladder-pos** the position of the ladder leading up.

**down-ladder-pos** the position of the ladder leading down.

**board-size** a number pair denoting the size of the board.

### status

A class for keeping track of statuses. Use the `make-status` function to create new statuses.

**duration** how long the status lasts for.

**on-applied** a function to call when the status is applied.

**on-update** a function to call every time the status is updated.

**target** the actor the status effect is active on.

### pickup

Inherits `actor`.
`consumable` is set to t and `solid` is set to nil by default.
Use the `make-pickup` function to create new pickups.

Interacting with a pickup adds the item to the player's inventory, if there's room for it.

The pickup's name, description, color, and display char are taken from its equipment.

**equipment** the equipment item that the pickup is for.

## game-engine.lisp

This file is called game-engine because it could theoretically be used as an engine behind other ASCII games. But not really.
Some of the most important functions and macros will be highlighted below.

**defenemy (name display-char new-slots &rest keys &key inherit &allow-other-keys)**

Creates a new enemy class with the name of `name`. It inherits `enemy` by default, but this can be overridden with the `:inherit` key.

The values passed to new-slots become new slots of the class. All other keys change the values of existing slots. The values of the `display-char` and `name` slots are automatically set.

A matching constructor function for the class is created. This function is of the form `make-name (pos)`, which creates an instance of the new class at the position `pos`.

Example: `(defenemy goblin #\g () :atk '(1 4 piercing) :health (1+ (roll 3)) :str -1 :dex 1 :color 'green :xp 3 :description "a goblin with a sharp dagger")` creates a class named 'goblin' and a function named 'make-goblin'. The atk, health, str, dex, color, xp, and description slots of the `goblin` class are set to the values entered in the `defenemy` call.

**defactor (name display-char new-slots &rest keys &key inherit &allow-other-keys)**

Works like `defenemy`, but inherits from `actor` by default. It also automatically sets the value of the `interact-action-only` slot if it's not provided.

Several actor classes are automatically defined. These are the corpse, bones, ladder, and secret-door classes.

**defequipment (name new-slots &rest &key inherit identifiedp &allow-other-keys)**

Also similar to `defenemy`, except it defines an extra function called `make-name-pickup`, which makes a pickup of the new class at the position passed to the function.

The weapon class is automatically defined. Its `weaponp` slot is set to t by default, and it has additional 'statuses' and 'onetime-effects' slots.

**make-status (duration &key on-update on-applied)**

Makes a new status effect with given duration and update/applied functions.

**make-layer (dungeon)**

Accepts as an argument an instance of the `dungeon` class from bsp-dungeon and turns it into an instance of `layer` from class-definitions.

**make-pickup (equipment pos)**

Makes an instance of the `pickup` class, at the given position, for the given equipment.