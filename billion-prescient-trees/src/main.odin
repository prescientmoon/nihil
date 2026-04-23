package anima

import "core:log"
import "core:fmt"
import "core:math"
import "base:intrinsics"

main :: proc() {
  context.logger = log.create_console_logger()
  stats := explore_whole()
  fmt.println(stats)
}

TURN_COUNT :: 3
Score :: distinct int

// {{{ Math helpers
choose :: #force_inline proc "contextless" (n, k: uint) -> uint {
  return uint(math.binomial(int(n), int(k)))
}

// Computes the next integer with the same number of bits set to one. Taken from
// some stack overflow page I forgot to link many years ago.
snoob :: proc(x: uint) -> uint {
  // x = xxx0 1111 0000
  //     0000 0001 0000
  smallest := x & uint(-int(x))
  //     xxx1 0000 0000
  ripple := x + smallest
  //     0001 1111 0000
  ones := x ~ ripple
  //     0000 0000 0111
  shifted_ones := ones >> (2 + intrinsics.count_trailing_zeros(smallest))
  //     xxx1 0000 0111
  return ripple | shifted_ones
}
// }}}
// {{{ Rigid decoding lookup tables
RIGID_BITS :: 12
RIGID_BIT_CASES :: RIGID_BITS + 1 // the 0 case also exists
RIGID_ENCODING_COUNT :: 1 << RIGID_BITS

RIGID_OFFSETS: [RIGID_BIT_CASES]uint
RIGID_LENGTHS: [RIGID_BIT_CASES]uint
RIGID_ENCODE:        [RIGID_ENCODING_COUNT]uint
RIGID_DECODE:        [RIGID_ENCODING_COUNT]uint

@init
compute_rigid_tables :: proc "contextless" () {
  for i in 1..<uint(RIGID_BIT_CASES) {
    stride := choose(RIGID_BITS, i - 1)
    RIGID_OFFSETS[i] = RIGID_OFFSETS[i - 1] + stride
  }

  for decoded in 0..<uint(RIGID_ENCODING_COUNT) {
    count := intrinsics.count_ones(decoded)
    encoded := RIGID_LENGTHS[count]
    RIGID_DECODE[RIGID_OFFSETS[count] + encoded] = decoded
    RIGID_ENCODE[decoded] = encoded
    RIGID_LENGTHS[count] += 1
  }

  for i in 0..<RIGID_BIT_CASES - 1 {
    assert_contextless(
      RIGID_OFFSETS[i + 1] - RIGID_OFFSETS[i] == RIGID_LENGTHS[i]
    )
  }
}
// }}}
// {{{ Bitset operations
bitset_to_integer :: #force_inline proc(bs: bit_set[$E], $I: typeid) -> I {
  transmuted := transmute(intrinsics.type_bit_set_underlying_type(bit_set[E]))bs
  return cast(I)transmuted
}

bitset_head :: #force_inline proc(bs: bit_set[$E]) -> (e: E, ok: bool) {
  tzs := intrinsics.count_trailing_zeros(bitset_to_integer(bs, int))
  if tzs >= len(E) do return
  return E(tzs), true
}

bitset_indexof :: #force_inline proc(
  bs: bit_set[$E], e: E
) -> (ix: uint, ok: bool) {
  return intrinsics.count_ones(
    bitset_to_integer(bs, uint) & ((1 << uint(e)) - 1)
  ), e in bs
}

bitset_index :: #force_inline proc(
  bs: bit_set[$E], target: uint
) -> (e: E, ok: bool) {
  for i: uint = 0; b in bs {
    if i == target do return b, true
    else do i += 1
  }

  return
}

Bitset_Fixed_Size_Subset_Iter :: struct($E: typeid) {
  remaining:     int,
  current:       uint,
  possibilities: bit_set[E],
}

bitest_fixed_size_subset_iter :: #force_inline proc(
  bs: bit_set[$E], #any_int ones: uint
) -> Bitset_Fixed_Size_Subset_Iter(E) {
  log.assert(ones <= 31)

  return {
    possibilities = bs,
    remaining     = math.binomial(card(bs), int(ones)),
    current       = (1 << ones) - 1,
  }
}

bitest_fixed_size_subset_next :: #force_inline proc(
  iter: ^Bitset_Fixed_Size_Subset_Iter($E)
) -> (set: bit_set[E], ok: bool) {
  (iter.remaining > 0) or_return
  set = decode_subset(iter.possibilities, iter.current)
  iter.remaining -= 1
  iter.current = snoob(iter.current)
  return set, true
}

// Inverse of `encode_subset`.
//
// Takes two bitfields as input, and produces a new bitfield
// by taking the position `k` of each `1` in `encoded`,
// and setting the `j`-th bit of the result to `1`,
// where `j` is the position of the `k`-th `1` in `other`.
decode_subset :: #force_inline proc(
  superset: bit_set[$E], encoded: uint,
) -> (out: bit_set[E]) {
  for i: uint = 0; e in superset {
    if (1 << i) & encoded != 0 do out += { e }
    i += 1
  }

  log.assert(out <= superset)
  return out
}

// Inverse of `encode_rigid`
decode_rigid :: #force_inline proc(
  encoded, ones: uint
) -> (decoded: uint, ok: bool) {
  ix := RIGID_OFFSETS[ones] + encoded
  return RIGID_DECODE[ix], encoded < RIGID_LENGTHS[ones]
}

// Inverse of `encode_rigid_subset`
decode_rigid_subset :: #force_inline proc(
  superset: bit_set[$E], encoded, ones: uint
) -> (decoded: bit_set[E], ok: bool) {
  flex: uint
  flex, ok = decode_rigid(encoded, ones)
  return decode_subset(superset, flex), ok
}
// }}}
// {{{ Creature
Creature :: enum u8 {
  None = 0, // Sentinel value
  Wall,
  Seer,
  Rogue,
  Bard,
  Diplomat,
  Ranger,
  Steward,
  Barbarian,
  Witch,
  Mercenary,
  Monarch,
}

CREATURES: Creatures : ~{ .None }
Creatures :: bit_set[Creature]

@rodata
CREATURE_STRENGTH := [Creature]int{
  .None      = 0,
  .Wall      = 0,
  .Seer      = 0,
  .Rogue     = 1,
  .Bard      = 2,
  .Diplomat  = 2,
  .Ranger    = 2,
  .Steward   = 2,
  .Barbarian = 3,
  .Witch     = 3,
  .Mercenary = 4,
  .Monarch   = 6,
}
// }}}
// {{{ Edict
Edict :: enum u8 {
  None = 0, // Sentinel value

  // Victory point edicts
  RileThePublic,
  DivertAttention,

  // Strength edicts
  Sabotage,
  Gambit,
  Ambush,
}

Edicts :: bit_set[Edict]
EDICTS: Edicts : ~{ .None }
// }}}
// {{{ Battlefield
Battlefield :: enum u8 {
  Mountain,
  Glade,
  Urban,
  Night,
  Plains,
  LastStrand,
}

@rodata
BATTLEFIELD_BONUS := [Battlefield]Creatures {
  .Mountain   = { .Ranger, .Barbarian, .Mercenary },
  .Glade      = { .Bard, .Ranger, .Witch },
  .Urban      = { .Rogue, .Bard, .Diplomat, .Steward },
  .Night      = { .Seer, .Rogue, .Ranger },
  .Plains     = {},
  .LastStrand = {},
}

@rodata
BATTLEFIELD_REWARD := [Battlefield]int{
  .Mountain   = 3,
  .Glade      = 3,
  .Urban      = 3,
  .Night      = 3,
  .Plains     = 3,
  .LastStrand = 5,
}
// }}}
// {{{ Effect
Effect :: enum u8 {
  // === Effects caused by battlefields:
  // The player gains 1 strength
  Mountain,
  // The player gains +2 vp if they win this battle
  Glade,
  // The player gains +1 vp if they win this batttle
  Night,

  // === Effects caused by creatures:
  // The player gets to play two creatures instead of one
  Seer,
  // The player gains 1 strength and gains
  // an additional point by winning this battle
  Bard,
  // This battle, lose 1 strength
  Mercenary,
  // The barbarian gains 2 strength if
  // it gets played
  Barbarian,
}

Effects :: bit_set[Effect]
// }}}
// {{{ Player
Player :: enum { Me, You }

@rodata
OPPONENT := [Player]Player {
  .Me  = .You,
  .You = .Me,
}

arrange :: #force_inline proc(player: Player, a, b: $T) -> (out: [Player]T) {
  out[player] = a
  out[OPPONENT[player]] = b
}
// }}}
// {{{ Known state
// Public information about a player
Known_Player_State :: struct {
  edicts:  Edicts,
  effects: Effects,
}

// State known by both players at some point in time.
Known_State :: struct {
  score:        Score,
  players:      [Player]Known_Player_State,
  battlefields: [TURN_COUNT]Battlefield,
  graveyard:    Creatures,
  turn:         u8,
}

hand_size :: #force_inline proc(state: Known_State) -> uint {
  return uint(card(CREATURES - state.graveyard) - 1) / 2
}

creature_choice_size :: #force_inline proc(
  state: Known_State, player: Player
) -> uint {
  return 1 + uint(.Seer in state.players[player].effects)
}
// }}}
// {{{ Choices
// Choice made by one of the players in the main phase
Main_Phase_Choice :: struct {
  creature:      Creature,
  seer_creature: Creature, // None => no seer effect is active
  edict:         Edict,
}

// Similar to Main_Phase_Choice but used after the seer phase gets resolved
Final_Main_Phase_Choice :: struct {
  creature: Creature,
  edict:    Edict,
}

// The creature the player called out for the Sabotage effect.
// None => no sabotage effect active.
Sabotage_Phase_Choice :: Creature

// The creature the player chose to keep.
// None => no seer effect active.
Seer_Phase_Choice     :: Creature
// }}}
// {{{ Battle
// The battle state after all the choices for the turn have been made (but not
// resolved).
Battle_Setup :: struct {
  using state:      Known_State,
  main_choices:     [Player]Final_Main_Phase_Choice,
  sabotage_choices: [Player]Sabotage_Phase_Choice,
}

Battle :: struct {
  using setup: Battle_Setup,

  // ========== Derived state computed from the data above
  battlefield: Battlefield,
  // Which players played advantaged creatures?
  battefield_bonuses: bit_set[Player],
  // Which players got their creature's effect negated?
  negated_creatures:  bit_set[Player],
  // Creatures that have not had their effects negated.
  active_creatures: [Player]Creature,
  // Used for the Steward and Urban effects
  edict_multipliers: [Player]int,
  // The person who won this turn's battle (but not necessarily the war)
  winner: Battle_Winner,
}

Turn_Outcome :: union { Score, Known_State }
Battle_Winner :: enum u8 { Tied, Me, You }

@rodata
BATTLE_WINNER := [Player]Battle_Winner {
  .Me  = .Me,
  .You = .You
}
// }}}
// {{{ Battle resolution
played_creature :: #force_inline proc(
  battle: Battle, player: Player
) -> Creature {
  return battle.main_choices[player].creature
}

played_edict :: #force_inline proc(battle: Battle, player: Player) -> Edict {
  return battle.main_choices[player].edict
}

// Computes the strength modifier for the creature a given player has played.
strength_modifier :: proc(
  battle: Battle, player: Player
) -> (out: int) {
  effects := battle.players[player].effects

  if player in battle.battefield_bonuses do out += 2

  // Creature strength bonuses:
  #partial switch battle.active_creatures[player] {
  case .Ranger: // [[[RANGER EFFECT 1]]]
    if battle.battefield_bonuses == { player } do out += 2
  case .Barbarian: // [[[BARBARIAN EFFECT 1]]]
    if .Barbarian in effects do out += 2
  }

  // Edict strength bonuses:
  // (the witch cannot get strength bonuses from edicts)
  // [[[WITCH EFFECT 2]]]
  if played_creature(battle, player) != .Witch {
    edict_bonus: int

    #partial switch played_edict(battle, player) {
    case .Sabotage: // [[[SABOTAGE EFFECT 1]]]
      opp_creature := played_creature(battle, OPPONENT[player])
      if battle.sabotage_choices[player] == opp_creature do edict_bonus = 3
    case .Ambush: // [[[AMBUSH EFFECT 1]]]
      if player in battle.battefield_bonuses do edict_bonus = 1
    case .Gambit: // [[[GAMBIT EFFECT 1]]]
      edict_bonus = 1
    }

    out += battle.edict_multipliers[player] * edict_bonus
  }

  // Lingering effects which modify strength:
  // Effects caused by the previously played creature
  if      .Bard      in effects do out += 1 // [[[BARD EFFECT 1]]]
  else if .Mercenary in effects do out -= 1 // [[[MERCENARY EFFECT 1]]]

  // Effects caused by previous battlefields
  // [[[MOUNTAIN EFFECT 1]]]
  if .Mountain in effects do out += 1

  return out
}

// Computes the strength modifier for the creature a given player has played.
strength_modifier_branchless :: proc(
  battle: Battle, player: Player
) -> (out: int) {
  effects := battle.players[player].effects

  out += 2 * int(player in battle.battefield_bonuses)

  // Creature strength bonuses:
  out += 2 * int(
    battle.active_creatures[player] == .Ranger &&
      battle.battefield_bonuses == { player } ||
    battle.active_creatures[player] == .Barbarian &&
      .Barbarian in effects
  )

  // Edict strength bonuses:
  // (the witch cannot get strength bonuses from edicts)
  // [[[WITCH EFFECT 2]]]
  opp_creature := played_creature(battle, OPPONENT[player])
  edict := played_edict(battle, player)
  out += 
    int(played_creature(battle, player) != .Witch) *
    battle.edict_multipliers[player] *
    (
      3 * int(
        edict == .Sabotage && 
          battle.sabotage_choices[player] == opp_creature
      ) + int(
        edict == .Ambush && 
          player in battle.battefield_bonuses ||
        edict == .Gambit
      )
    )

  // Lingering effects which modify strength:
  // Effects caused by the previously played creature
  out += int(.Bard in effects) - int(.Mercenary in effects)

  // Effects caused by previous battlefields
  // [[[MOUNTAIN EFFECT 1]]]
  out += int(.Mountain in effects)

  return out
}

// Calculates the amount of victory points
// earned by winning this partidcular battle
// as a given player.
battle_reward :: #force_inline proc(
  battle: Battle, player: Player
) -> (total: int) {
  effects := battle.players[player].effects
  total = BATTLEFIELD_REWARD[battle.battlefield]

  // Lingering effects:
  if      .Night in effects do total += 1 // [[[NIGHT EFFECT 1]]]
  else if .Glade in effects do total += 2 // [[[GLADE EFFECT 1]]]

  // [[[BARD EFFECT 2]]]
  if .Bard in effects do total += 1

  for player in Player {
    mine  := played_edict(battle, player)
    yours := played_edict(battle, OPPONENT[player])
    mult  := battle.edict_multipliers[player]

    // Apply the "rile the public" and "divert attention" edicts.
    if mine == .RileThePublic {
      // [[[RILETHEPUBLIC EFFECT 1]]]
      total += mult
    } else if mine == .DivertAttention && yours != .RileThePublic {
      // [[[DIVERTATTENTION EFFECT 1]]]
      // [[[RILETHEPUBLIC EFFECT 2]]]
      total -= mult
    }
  }

  // This is the only place where the total can decrease,
  // which is why we must be careful for it not to become negative.
  return max(0, total)
}


advance_known_state :: proc(setup: Battle_Setup) -> Turn_Outcome {
  battle := Battle { setup = setup }

  // 1. Compute battlefield bonuses
  battle.battlefield = battle.battlefields[battle.turn]
  advantaged_creatures := BATTLEFIELD_BONUS[battle.battlefield]
  for player in Player {
    creature := played_creature(battle, player)

    if creature in advantaged_creatures {
      battle.battefield_bonuses += { player }
    }
  }

  // 2. Compute active/negated creatures
  for player in Player {
    mine  := played_creature(battle, player)
    yours := played_creature(battle, OPPONENT[player])

    // [[[WITCH EFFECT 1]]]
    witch := yours == .Witch
    // [[[ROGUE EFFECT 1]]]
    rogue := mine == .Seer && yours == .Rogue;

    if witch || rogue {
      battle.negated_creatures += { player }
    } else {
      battle.active_creatures[player] = mine
    }
  }

  // 3. Compute edict multipliers
  for player in Player {
    mult: int = 1

    // [[[URBAN EFFECT 1]]]
    mult += int(battle.battlefields[battle.turn] == .Urban)
    // [[[STEWARD EFFECT 1]]]
    mult += int(battle.active_creatures[player] == .Steward)

    battle.edict_multipliers[player] = mult
  }

  // 4. Resolve wins by card effects
  for player in Player {
    yours := played_creature(battle, OPPONENT[player])
    won: bool

    #partial switch battle.active_creatures[player] {
    case .Witch: // [[[WITCH EFFECT 3]]]
      won = yours == .Wall
    case .Rogue: // [[[ROGUE EFFECT 2]]] 
      won = yours == .Monarch || yours == .Wall
    case .Diplomat: // [[[DIPLOMAT EFFECT 1]]]
      // The diplomat wins against any creature
      // if the two edicts are identical
      won = battle.main_choices[.Me].edict == battle.main_choices[.You].edict
    }

    if won {
      battle.winner = BATTLE_WINNER[player]
      break // The two players can never win at the same time
    }
  }

  // 5. Pick a winner for the actual battle
  mine  := played_creature(battle, .Me)
  yours := played_creature(battle, .You)

  // The wall can force ties.
  // We don't have to check for the wall being negated here,
  // as that would trigger a win by effect.
  // [[[WALL EFFECT 1]]]
  if mine != .Wall && yours != .Wall {
    strengths := [2]int{
      CREATURE_STRENGTH[mine]  + strength_modifier_branchless(battle, .Me),
      CREATURE_STRENGTH[yours] + strength_modifier_branchless(battle, .You),
    }

    switch {
    case strengths[0] > strengths[1]: battle.winner = .Me
    case strengths[0] < strengths[1]: battle.winner = .You
    }
  }

  // 6. Resolve the gambit effect in case of ties
  if battle.winner == .Tied { // [[[GAMBIT EFFECT 2]]]
    mine  := played_edict(battle, .Me)
    yours := played_edict(battle, .You)

    switch {
    // If both players played gambits, nothing happens
    case mine == yours: // still a tie!
    // if we played a gambit, we lose on ties
    case mine == .Gambit: battle.winner = .You
    // if the opponent has played a gambit, they lose on ties
    case yours == .Gambit: battle.winner = .Me
    }
  }

  // 7. Resolve the point totals
  delta := 0
  #partial switch battle.winner {
  case .Me:  delta += battle_reward(battle, .Me)
  case .You: delta -= battle_reward(battle, .You)
  }

  // The reward for a player killing (or tying) the monarch
  for player in Player {
    op := OPPONENT[player]
    if battle.winner != BATTLE_WINNER[op] {
      if battle.active_creatures[op] == .Monarch { // [[[MONARCH EFFECT 1]]]
        switch player {
        case .Me: delta += 2
        case .You: delta -= 2
        }
      }
    }
  }

  // 8. Move onto the next turn
  score := battle.score + Score(delta)
  if battle.turn == TURN_COUNT - 1 do return score

  future: Known_State = battle.state
  future.turn += 1
  future.score = score

  // Discard used edicts
  for player in Player {
    future.players[player].edicts -= { played_edict(battle, player) }
  }

  // Kill creatures off
  for player in Player {
    future.graveyard += { played_creature(battle, player) }
  }

  // Clear status effects
  for player in Player do future.players[player].effects = {}

  for player in Player {
    if battle.active_creatures[player] == .Steward { // [[[STEWARD EFFECT 2]]]
      // Bounce every edict back to hand
      future.players[player].edicts = EDICTS
      break
    }
  }

  // Set up global lingering effects
  if battle.battlefield == .Night { // [[[NIGHT SETUP]]]
    for player in Player do future.players[player].effects += { .Night }
  }

  // first is winner, second is loser
  switch battle.winner {
  case .Tied:
  case .Me, .You:
    winner: Player = battle.winner == .Me ? .Me : .You
    loser := OPPONENT[winner]

    #partial switch battle.battlefield {
    case .Glade: // [[[GLADE SETUP]]]
      future.players[winner].effects += { .Glade }
    case .Mountain: // [[[MOUNTAIN SETUP]]]
      future.players[winner].effects += { .Mountain }
    }

    // if this card has already been played there's no point
    // in adding the status effect anymore
    if .Barbarian not_in future.graveyard { // [[[BARBARIAN SETUP]]]
      future.players[loser].effects += { .Barbarian }
    }
  };

  for player in Player {
    #partial switch battle.active_creatures[player] {
    case .Mercenary: // [[[MERCENARY SETUP]]]
      future.players[player].effects += { .Mercenary }
    case .Seer: // [[[SEER SETUP]]]
      future.players[player].effects += { .Seer }
    case .Bard: // [[[BARD SETUP]]]
      future.players[player].effects += { .Bard }
    }
  }

  return future
}
// }}}
// {{{ Battle tree exploration
Exploration_Stats :: struct {
  initial_count:  uint,
  terminal_count: uint,
  main_count:     uint,
  sabotage_count: uint,
  seer_count:     uint,
  win_counts:     [Battle_Winner]uint,
}

Exploration_State :: struct {
  using known: Known_State,
  hands: [Player]Creatures,
  stats: ^Exploration_Stats,
}

Player_Main_Choices_Iter :: struct {
  hand:        Creatures,
  edicts:      Edicts,
  last_choice: Main_Phase_Choice,
  seer:        bool,

  remaining_edicts:    Edicts,
  remaining_hand:      Creatures,
  remaining_seer_hand: Creatures,
}

main_choices_iter :: #force_inline proc(
  state: Exploration_State, player: Player,
) -> Player_Main_Choices_Iter {
  return {
    hand             = state.hands[player],
    remaining_hand   = state.hands[player],
    edicts           = state.players[player].edicts,
    remaining_edicts = state.players[player].edicts,
    seer             = .Seer in state.players[player].effects,
  }
}

main_choices_next_edict :: #force_inline proc(
  iter: ^Player_Main_Choices_Iter
) -> (edict: Edict, ok: bool) {
  edict = bitset_head(iter.remaining_edicts) or_return
  iter.remaining_edicts -= { edict }
  return edict, true
}

main_choices_next_creature :: #force_inline proc(
  iter: ^Player_Main_Choices_Iter
) -> (creature: Creature, ok: bool) {
  creature = bitset_head(iter.remaining_hand) or_return
  iter.remaining_hand -= { creature }
  return creature, true
}

main_choices_next_seer_creature :: #force_inline proc(
  iter: ^Player_Main_Choices_Iter
) -> (creature: Creature, ok: bool) {
  creature = bitset_head(iter.remaining_seer_hand) or_return
  iter.remaining_seer_hand -= { creature }
  return creature, true
}

main_choices_next :: proc(
  iter: ^Player_Main_Choices_Iter
) -> (choice: Main_Phase_Choice, ok: bool) {
  defer if ok do iter.last_choice = choice
  first := iter.last_choice.edict == .None

  if seer_creature, ok := main_choices_next_seer_creature(iter); first || !ok {
    if creature, ok := main_choices_next_creature(iter); first || !ok {
      edict := main_choices_next_edict(iter) or_return
      iter.last_choice.edict = edict
      iter.remaining_hand = iter.hand
    } else {
      iter.last_choice.creature = creature
      if iter.seer {
        iter.remaining_seer_hand = iter.remaining_hand
      } else {
        iter.remaining_seer_hand = { .None }
      }
    }

    return main_choices_next(iter)
  } else {
    choice = iter.last_choice
    choice.seer_creature = seer_creature
    return choice, true
  }
}

player_sabotage_choices :: #force_inline proc(
  state: Exploration_State, mp: Main_Phase_Choice,
) -> Creatures {
  if mp.edict == .Sabotage {
    return CREATURES - state.graveyard
  } else {
    return { .None }
  }
}

player_seer_choices :: #force_inline proc(
  state: Exploration_State, mp: Main_Phase_Choice,
) -> Creatures {
  if mp.seer_creature == .None {
    return { mp.creature }
  } else {
    log.assertf(mp.creature != mp.seer_creature, "Invalid choice: %v", mp)
    return { mp.creature, mp.seer_creature }
  }
}

explore_sabotage :: #force_inline proc(
  state: Exploration_State, mp: [Player]Main_Phase_Choice
) {
  state.stats.sabotage_count += 1
  for c1 in player_sabotage_choices(state, mp[.Me]) {
    for c2 in player_sabotage_choices(state, mp[.You]) {
      explore_seer(state, mp, { .Me = c1, .You = c2})
    }
  }
}

explore_seer :: #force_inline proc(
  state: Exploration_State,
  mp: [Player]Main_Phase_Choice,
  sabotage: [Player]Sabotage_Phase_Choice
) {
  state.stats.seer_count += 1
  for c1 in player_seer_choices(state, mp[.Me]) {
    for c2 in player_seer_choices(state, mp[.You]) {
      setup := Battle_Setup {
        state            = state.known,
        main_choices     = {
          .Me  = { c1, mp[.Me].edict },
          .You = { c2, mp[.You].edict }
        },
        sabotage_choices = sabotage,
      }

      switch inner in advance_known_state(setup) {
      case Score:
        state.stats.terminal_count += 1
        switch {
        case inner > 0: state.stats.win_counts[.Me] += 1
        case inner < 0: state.stats.win_counts[.You] += 1
        case: state.stats.win_counts[.Tied] += 1
        }

      case Known_State:
        next := state
        next.known = inner
        for p in Player do next.hands[p] -= { setup.main_choices[p].creature }
        explore_main(next)
      }
    }
  }
}

explore_main :: proc(state: Exploration_State) {
  state.stats.main_count += 1
  count1 := main_phase_index_count(state.known, .Me)
  count2 := main_phase_index_count(state.known, .You)
  for ix1 in 0..<count1 {
    c1, ok := decode_main_index(ix1, state.known, .Me, state.hands[.Me])
    log.assert(ok)
    for ix2 in 0..<count2 {
      c2, ok := decode_main_index(ix2, state.known, .You, state.hands[.You])
      log.assert(ok)
      explore_sabotage(state, { .Me = c1, .You = c2 })
    }
  }
}

explore_whole :: proc() -> (stats: Exploration_Stats) {
  all_battlefields: [4]Battlefield = { .Glade, .Urban, .Night, .LastStrand }
  battlefields: [TURN_COUNT]Battlefield
  for i in 0..<TURN_COUNT do battlefields[i] = all_battlefields[i]

  for removed in CREATURES {
    iter := bitest_fixed_size_subset_iter(CREATURES - { removed }, 5)
    for hand in bitest_fixed_size_subset_next(&iter) {
      other_hand := CREATURES - { removed } - hand

      state := Exploration_State {
        stats = &stats,
        hands = { .Me = hand, .You = other_hand },
        battlefields = battlefields,
        players = {
          .Me  = { edicts = EDICTS },
          .You = { edicts = EDICTS },
        },
      }

      stats.initial_count += 1
      explore_main(state)
      return stats // Enough for now :p
    }
  }

  return stats
}
// }}}
// {{{ Arithmetic encoding
// Embed an integer inside self given the maximum value of the integer.
mix_ranged :: #force_inline proc(self, value, max: uint) -> uint {
  return max * self + value
}

// The inverse of mix_ranged.
unmix_ranged :: #force_inline proc(self, max: uint) -> (out, vaule: uint) {
  return self / max, self % max
}

// Mix in data about the index of some bit in a bitfield.
mix_indexof :: #force_inline proc(
  self: uint, element: $T, possibilities: bit_set[T]
) -> (res: uint, ok: bool) {
  index: uint
  index, ok = bitset_indexof(possibilities, element)
  return mix_ranged(self, res, card(possibilities)), ok
}

// Inverse of `mix_indeof`
unmix_indexof :: #force_inline proc(
  self: uint, possibilities: bit_set[$T]
) -> (res: uint, element: T, ok: bool) {
  remaining, index := unmix_ranged(self, uint(card(possibilities)))
  element, ok = bitset_index(possibilities, index)
  return remaining, element, ok
}
// }}}
// {{{ Decision indices
Decision_Index :: uint

// Decodes a main phase user choice into a decision index.
decode_main_index :: proc(
  index: Decision_Index,
  state: Known_State,
  player: Player,
  hand: Creatures,
) -> (choice: Main_Phase_Choice, ok: bool) {
  log.assert(uint(card(hand)) == hand_size(state))

  encoded_creatures, edict := unmix_indexof(
    index,
    state.players[player].edicts
  ) or_return
  choice.edict = edict

  creature_choice := decode_rigid_subset(
    hand,
    encoded_creatures,
    creature_choice_size(state, player),
  ) or_return;

  creature, cok := bitset_head(creature_choice)
  log.assert(cok)
  choice.creature = creature

  choice.seer_creature, _ =  bitset_head(creature_choice - { creature })
  log.assert(creature_choice - { creature, choice.seer_creature } == {})

  return choice, true
}

main_phase_index_count :: proc(state: Known_State, player: Player) -> uint {
  choice_count := choose(
    hand_size(state),
    creature_choice_size(state, player)
  )

  edict_count := uint(card(state.players[player].edicts))

  return choice_count * edict_count
}
// }}}
