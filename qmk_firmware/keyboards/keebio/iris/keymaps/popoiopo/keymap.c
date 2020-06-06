#include QMK_KEYBOARD_H

extern keymap_config_t keymap_config;

#define _QWERTY 0
#define _LOWER 1
#define _RAISE 2
#define _ADJUST 3

enum custom_keycodes {
  QWERTY = SAFE_RANGE,
  LOWER,
  RAISE,
  ADJUST,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

  [_QWERTY] = LAYOUT(
  //┌──────────────┬────────┬────────┬────────┬────────┬────────┐                          ┌────────┬────────┬────────┬────────┬────────┬────────┐
     KC_ESC,        KC_1,    KC_2,    KC_3,    KC_4,    KC_5,                               KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_BSPC,
  //├──────────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼───\────┤
     KC_TAB,        KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,                               KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSLS,
  //├──────────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼───;────┼───'────┤
     KC_LCTL,       KC_A,    KC_S,    KC_D,    KC_F,    KC_G,                               KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
  //├──────────────┼────────┼────────┼────────┼────────┼────────┼────────┐        ┌────────┼────────┼────────┼───,────┼───.────┼───/────┼────────┤
     SFT_T(KC_CAPS),KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    KC_MUTE,          ADJUST,  KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_DEL,
  //└──────────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘
                                          KC_LCMD, KC_LOPT,   KC_ENT,                    KC_SPC,  LOWER,   RAISE
                                      // └────────┴────────┴────────┘                 └────────┴────────┴────────┘
		     ),

  
  [_LOWER] = LAYOUT(
  //┌────────┬───?────┬────────┬────────┬────────┬────────┐                          ┌────────┬──-_────┬──=+────┬────────┬────────┬────────┐
     _______, KC_QUES, _______, _______, _______, _______,                            _______, KC_MINS, KC_EQL,  KC_7,    KC_8,    KC_9,   
  //├───"────┼───{────┼───}────┼───+────┼───%────┼────^───┤                          ├───~`───┼──[{────┼───]}───┼────────┼────────┼────────┤
     KC_DQT,  KC_LCBR, KC_RCBR, KC_PLUS, KC_PERC, KC_CIRC,                            KC_TILD, KC_LBRC, KC_RBRC, KC_4,    KC_5,    KC_6,   
  //├────────┼───(────┼───)────┼───-────┼───_────┼────&───┤                          ├───;:───┼──'"────┼───\|───┼────────┼────────┼────────┤
     _______, KC_LPRN, KC_RPRN, KC_MINS, KC_UNDS, KC_AMPR,                            KC_SCLN, KC_QUOT, KC_BSLS, KC_1,    KC_2,    KC_3,   
  //├────────┼───<────┼───>────┼───=────┼───*────┼────|───┼────────┐        ┌────────┼───,<───┼──.>────┼───/?───┼────────┼────────┼────────┤
     _______, KC_LT,   KC_GT,   KC_EQL,  KC_ASTR, KC_PIPE, KC_MUTE,          _______, KC_COMM, KC_DOT,  KC_SLSH, KC_PLUS, KC_0,    KC_MINS,
  //└────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘
                                    _______, _______, _______,                   _______, _______, _______
                                // └────────┴────────┴────────┘                 └────────┴────────┴────────┘
		    ),


  [_RAISE] = LAYOUT(
  //┌────────┬────────┬────────┬────────┬────────┬────────┐                          ┌────────┬────────┬────────┬────────┬────────┬────────┐
     KC_TILD, KC_EXLM, KC_AT,   KC_HASH, KC_DLR,  KC_PERC,                            KC_CIRC, KC_AMPR, KC_ASTR, KC_LPRN, KC_RPRN, KC_PGUP,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼───────
     _______, KC_7,    KC_8,    KC_9,    KC_PLUS, _______,                            _______, _______, _______, _______, KC_UNDS, KC_PGDN,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     _______, KC_4,    KC_5,    KC_6,    KC_MINS, _______,                            _______, KC_HOME, _______, _______, _______, _______,
  //├────────┼────────┼────────┼────────┼────────┼────────┼────────┐        ┌────────┼────────┼────────┼────────┼────────┼────────┼────────┤
     KC_0,    KC_1,    KC_2,    KC_3,    KC_EQL,  _______, KC_MUTE,          _______, _______, _______, _______, _______, _______, KC_END,
  //└────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘
                                    _______, _______, _______,                   KC_DEL,  KC_BSPC, _______
                                // └────────┴────────┴────────┘                 └────────┴────────┴────────┘
		   ),
   
      
  [_ADJUST] = LAYOUT(
  //┌────────┬────────┬────────┬────────┬────────┬────────┐                          ┌────────┬────────┬────────┬────────┬────────┬────────┐
     RESET  , _______, _______, _______, _______, _______,                            KC_BTN1, KC_BTN2, KC_ACL0,  KC_ACL1, KC_ACL2,RGB_TOG,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     _______, KC_7,    KC_8,    KC_9,    KC_EQL,  _______,                            KC_MS_L,  KC_MS_D, KC_MS_U, KC_MS_R,RGB_SAD, RGB_SAI,
  //├────────┼────────┼────────┼────────┼────────┼────────┤                          ├────────┼────────┼────────┼────────┼────────┼────────┤
     _______, KC_4,    KC_5,    KC_6,    KC_PLUS, _______,                            KC_LEFT, KC_DOWN, KC_UP,   KC_RIGHT,RGB_VAD, RGB_VAI,
  //├────────┼────────┼────────┼────────┼────────┼────────┼────────┐        ┌────────┼────────┼────────┼────────┼────────┼────────┼────────┤
     KC_0,    KC_1,    KC_2,    KC_3,    KC_MINS, _______, KC_MUTE,          _______, KC_MRWD, KC_MPLY, KC_MFFD, _______, RGB_HUD, RGB_HUI,
  //└────────┴────────┴────────┴───┬────┴───┬────┴───┬────┴───┬────┘        └───┬────┴───┬────┴───┬────┴───┬────┴────────┴────────┴────────┘
                                    _______, _______, _______,                   _______, _______, _______
                                // └────────┴────────┴────────┘                 └────────┴────────┴────────┘
		     )
};


bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    case QWERTY:
      if (record->event.pressed) {
        set_single_persistent_default_layer(_QWERTY);
      }
      return false;
      break;
    case LOWER:
      if (record->event.pressed) {
        layer_on(_LOWER);
        update_tri_layer(_LOWER, _RAISE, _ADJUST);
      } else {
        layer_off(_LOWER);
        update_tri_layer(_LOWER, _RAISE, _ADJUST);
      }
      return false;
      break;
    case RAISE:
      if (record->event.pressed) {
        layer_on(_RAISE);
        update_tri_layer(_LOWER, _RAISE, _ADJUST);
      } else {
        layer_off(_RAISE);
        update_tri_layer(_LOWER, _RAISE, _ADJUST);
      }
      return false;
      break;
    case ADJUST:
      if (record->event.pressed) {
        layer_on(_ADJUST);
      } else {
        layer_off(_ADJUST);
      }
      return false;
      break;
  }
  return true;
}

void encoder_update_user(uint8_t index, bool clockwise) {
  if(IS_LAYER_ON(_RAISE)) { // on Raise layer control volume
    if (clockwise){
      tap_code(KC_VOLD);
    } else{
      tap_code(KC_VOLU);
    }
  }else if(IS_LAYER_ON(_LOWER)) { // on Raise layer control volume
    if (clockwise){
      tap_code(KC_BRID);
    } else{
      tap_code(KC_BRIU);
    }
  } else if(IS_LAYER_ON(_ADJUST)) { // on Raise layer control volume
    if (clockwise){
      tap_code(KC_PGDN);
    } else{
      tap_code(KC_PGUP);
    }
  } else { // on other layers emulate mouse scrollwheel
    if (clockwise){
      tap_code(KC_MS_WH_DOWN);
    } else{
      tap_code(KC_MS_WH_UP);
    }
  }
}
