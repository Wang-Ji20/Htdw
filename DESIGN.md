# Project Design Document: Htdw (How to Design Worlds - Danmaku Shooter)

## 1. Executive Summary

**Htdw** is a 2D fixed-screen arcade/danmaku (curtain fire) shooter developed in idiomatic [Racket](https://racket-lang.org/) leveraging the `2htdp/universe` and `2htdp/image` functional game development framework. 

The game features Touhou Project characters:
- **Player**: Marisa Kirisame (`marisa.png`), an agile magician moving in 2D space and firing rapid upward magical projectiles.
- **Enemy**: Cirno (`cirno.png`), an ice fairy who periodically spawns and traverses the screen, bouncing off boundaries.

The core objective is to avoid colliding with enemies while shooting them down to maximize points.

---

## 2. Architecture & Design Principles

The project follows the functional reactive design pattern established in *How to Design Programs* (HtDP) and the `2htdp/universe` framework:

$$\text{State}_{t+1} = \text{tick}(\text{State}_t)$$

All game state is stored in immutable transparent data structures. State transitions produce a new state rather than mutating existing memory in-place.

```mermaid
flowchart TD
    subgraph Input & Events
        K[Key Press / Release]
        T[Clock Tick 28Hz]
    end

    subgraph World State [World State: Immutable Struct]
        P[Player]
        E[Enemies List]
        B[Projectiles List]
        S[Spawn Timer]
        Pts[Score]
    end

    subgraph Engine Pipeline
        WT[world-tick]
        KP[alter-player-on-key]
        KR[clear-player-velocity]
        CR[resolve-combat]
        DR[render / render-game-over]
    end

    T --> WT
    K --> KP
    K --> KR

    KP --> World State
    KR --> World State
    WT --> CR --> World State

    World State --> DR
    World State -->|stop-when lose?| GO[Game Over Screen]
```

### Idiomatic Racket Best Practices
- **Language**: `#lang racket` provides full standard library functionality.
- **Data Encapsulation**: Transparent structures `(struct ... #:transparent)` allow easy value-based equality, printing, and non-destructive updating via `struct-copy`.
- **Higher-Order Combinators**: Built-in `ormap`, `andmap`, `filter`, `foldl`, and `for/fold` replace imperative loops and hand-rolled list traversals.
- **Submodule Pattern**:
  - `(module+ main (run))`: Entry point for interactive gameplay; does not trigger during automated testing or module imports.
  - `(module+ test ...)`: Headless automated unit tests using `rackunit`, executed via `raco test plane.rkt`.

---

## 3. Data Model

```
+-----------------------------------------------------------+
|                          world                            |
| + player: player                                          |
| + enemies: (listof enemy)                                 |
| + projectiles: (listof projectile)                        |
| + enemy-spawn-cd: integer                                 |
| + points: integer                                         |
+-----------------------------------------------------------+
         |                      |                     |
         v                      v                     v
+------------------+   +------------------+  +-------------------+
|      player      |   |      enemy       |  |    projectile     |
| + velocity: vel  |   | + velocity: vel  |  | + velocity: vel   |
| + pos: posn      |   | + pos: posn      |  | + pos: posn       |
| + cd: integer    |   | + hp: integer    |  | + emitter: symbol |
+------------------+   +------------------+  +-------------------+
```

### Type Signatures
- **`posn`**: `(posn Real Real)` — Represents a 2D spatial coordinate $(x, y)$ on the 600x800 canvas.
- **`velocity`**: `(velocity Real Real)` — Represents displacement vectors $(\Delta x, \Delta y)$ per tick.
- **`player`**: 
  - `velocity`: Current movement vector.
  - `pos`: Center position of Marisa's sprite.
  - `cd`: Integer cooldown counter until the next projectile fires.
- **`enemy`**:
  - `velocity`: Movement vector (e.g., horizontal bounce vector).
  - `pos`: Center position of Cirno's sprite.
  - `hp`: Hit points (destroyed when depleted).
- **`projectile`**:
  - `velocity`: Travel vector (typically $(v_{player, x}, -40)$).
  - `pos`: Current coordinate.
  - `emitter`: Ownership symbol (`'player` or `'enemy`).
- **`world`**: Root aggregate containing all simulation components.

---

## 4. Subsystems

### 4.1 Player Movement & Boundary Clamping
- **Controls**: Supports both `WASD` and Arrow Keys (`up`, `down`, `left`, `right`).
- **Independent Axis Control**:
  - Pressing a direction key modifies only that axis's velocity component.
  - Releasing a key checks if the player is currently moving in that direction before resetting that axis to `0`. Releasing one key does not halt orthogonal movement, enabling fluid diagonal flight.
- **Screen Confinement**:
  Marisa's sprite dimensions ($26 \times 44$ px) are bounded by `confine-player-x` and `confine-player-y`:
  $$x \in [13, 587], \quad y \in [22, 778]$$
  This prevents the player from slipping off-screen.

### 4.2 Enemy Spawning & Traversal
- Enemies spawn at $(200, 120)$ every `ENEMY-SPAWN-CD` ticks ($45$ ticks $\approx 1.6$ s).
- Movement is directed horizontally at `ENEMY-SPEED` ($8$ px/tick).
- **Wall Bouncing**:
  Bouncing uses boundary detection combined with velocity sign validation:
  $$\text{Bounce } v_x \iff (x \le \text{half-width} \land v_x < 0) \lor (x \ge \text{WIDTH} - \text{half-width} \land v_x > 0)$$
  This ensures enemies bounce cleanly without wall-sticking glitches.

### 4.3 Combat Resolution & Collision Detection
- **Hitbox Model**: Circular hitboxes based on Euclidean distance squared:
  $$\Delta x^2 + \Delta y^2 < (r_1 + r_2)^2$$
  - `PLAYER-RADIUS`: 13 px
  - `ENEMY-RADIUS`: 22 px
  - `PROJECTILE-RADIUS`: 5 px
- **Combat Resolution (`resolve-combat`)**:
  - Evaluated using a functional `for/fold` accumulator.
  - Each player projectile can strike at most one enemy.
  - Upon impact, both the projectile and the enemy are consumed, and the score increments immediately by 1 point.
- **Game Over (`lose?`)**:
  - Triggers if the player comes within $(r_{player} + r_{enemy})$ of any enemy or within $(r_{player} + r_{proj})$ of any enemy projectile.
  - Displays a dedicated `GAME OVER` screen overlay with the final score.

### 4.4 Rendering Pipeline
Composes visual layers using `2htdp/image`:
1. `BACKGROUND`: $600 \times 800$ empty canvas.
2. `Projectiles`: Rendered via `foldl` with circular sprites.
3. `Enemies`: Rendered at enemy positions with Cirno's sprite.
4. `Player`: Rendered at player position with Marisa's sprite.
5. `HUD`: Current score displayed in yellow text at top center.
6. `Game Over Overlay`: Displayed upon defeat.

---

## 5. Current Status

| Component | Status | Details |
|---|---|---|
| **Language & Environment** | Completed | Converted from `#lang htdp/asl` to `#lang racket` |
| **Player Controls** | Completed | Smooth WASD + Arrow keys with diagonal movement and decoupled release |
| **Boundary Clamping** | Completed | Sprite-aware boundary clamping keeps Marisa inside view |
| **Projectile System** | Completed | Autonomous firing, inertial velocity inheritance, out-of-bounds culling |
| **Enemy Spawning & Motion**| Completed | Timed spawns, velocity-sign aware wall bouncing |
| **Hitbox & Combat** | Completed | Euclidean collision detection, projectile consumption, immediate scoring |
| **Game Over State** | Completed | Accurate distance-based collision detection with game over overlay |
| **Automated Testing** | Completed | 21 unit tests covering math, movement, collisions, and state steps |

### Summary of Bugs Resolved During Modernization
1. **ASL to Racket Migration**: Replaced teaching language constructs and custom helper re-implementations (`anyof`, `noneof`, `fold`, `allthat`, `math-square`) with standard Racket library functions (`ormap`, `andmap`, `foldl`, `count`, `sqr`).
2. **Comparison Safety**: Replaced unsafe `eq?` comparisons on numbers and lists with `=`, `<=`, `>=`, and `empty?`.
3. **Player Start Position**: Corrected start location from extreme bottom-right `(600, 800)` to centered near bottom `(300, 720)`.
4. **Key Release Freeze**: Fixed `on-release` resetting all velocity to 0, which previously broke diagonal movement and caused jerky stops.
5. **Wall Sticking Projectiles**: Removed screen-boundary clamping from projectiles, enabling clean off-screen culling without bullets sticking to the right wall.
6. **Projectile Consumption**: Bullets are now consumed upon striking an enemy rather than piercing infinitely.
7. **Loss Condition Detection**: Replaced exact coordinate equality `(equal? enemy-pos player-pos)` (which virtually never matched due to step velocities) with circular hitbox collision.
8. **Lingering Zombie Enemies**: Replaced the 1-tick delayed zombie counting mechanism with synchronous combat resolution, preventing dead enemies from persisting for a frame.
9. **GUI Test Decoupling**: Wrapped `big-bang` inside a `(module+ main ...)` runner so unit tests run cleanly in headless CI / CLI without launching GUI windows.

---

## 6. Future Roadmap

1. **Enemy Danmaku Patterns**:
   - Utilize the existing `emitter: 'enemy` projectile support to have Cirno fire targeted or radial ice crystal bullets.
2. **Multiple Enemy Archetypes**:
   - Introduce varied enemy waves with distinct movement trajectories (e.g., sine waves, diving sweeps).
3. **Audio Support**:
   - Integrate background music and sound effects (spell card firing, enemy destruction) via Racket's multimedia libraries.
4. **Focus Movement Mode**:
   - Implement Touhou-style "focus" mode (holding `Shift` reduces player speed and reveals a precise core hitbox).

