with agar.core.event;
with agar.core.slist;
with agar.core.timeout;
with agar.gui.surface;
with agar.gui.widget.menu;
with agar.gui.widget.scrollbar;
with agar.gui.window;

package agar.gui.widget.table is

  use type c.unsigned;

  type popup_t;
  type popup_access_t is access all popup_t;
  pragma convention (c, popup_access_t);

  txt_max      : constant := 128;
  fmt_max      : constant := 16;
  col_name_max : constant := 48; 

  type select_mode_t is (SEL_ROWS, SEL_CELLS, SEL_COLS);
   for select_mode_t use (SEL_ROWS => 0, SEL_CELLS => 1, SEL_COLS => 2);
   for select_mode_t'size use c.unsigned'size;
  pragma convention (c, select_mode_t);

  package popup_slist is new agar.core.slist
    (entry_type => popup_access_t);

  type popup_t is record
    m      : c.int;
    n      : c.int;
    menu   : agar.gui.widget.menu.menu_access_t;
    item   : agar.gui.widget.menu.item_access_t;
    panel  : agar.gui.window.window_access_t;
    popups : popup_slist.entry_t;
  end record;
  pragma convention (c, popup_t);

  type cell_type_t is (
    CELL_NULL,
    CELL_STRING,
    CELL_INT,
    CELL_UINT,
    CELL_LONG,
    CELL_ULONG,
    CELL_FLOAT,
    CELL_DOUBLE,
    CELL_PSTRING,
    CELL_PINT,
    CELL_PUINT,
    CELL_PLONG,
    CELL_PULONG,
    CELL_PUINT8,
    CELL_PSINT8,
    CELL_PUINT16,
    CELL_PSINT16,
    CELL_PUINT32,
    CELL_PSINT32,
    CELL_PFLOAT,
    CELL_PDOUBLE,
    CELL_INT64,
    CELL_UINT64,
    CELL_PINT64,
    CELL_PUINT64,
    CELL_POINTER,
    CELL_FN_SU,
    CELL_FN_TXT,
    CELL_WIDGET
  );
  for cell_type_t use (
    CELL_NULL    => 0,
    CELL_STRING  => 1,
    CELL_INT     => 2,
    CELL_UINT    => 3,
    CELL_LONG    => 4,
    CELL_ULONG   => 5,
    CELL_FLOAT   => 6,
    CELL_DOUBLE  => 7,
    CELL_PSTRING => 8,
    CELL_PINT    => 9,
    CELL_PUINT   => 10,
    CELL_PLONG   => 11,
    CELL_PULONG  => 12,
    CELL_PUINT8  => 13,
    CELL_PSINT8  => 14,
    CELL_PUINT16 => 15,
    CELL_PSINT16 => 16,
    CELL_PUINT32 => 17,
    CELL_PSINT32 => 18,
    CELL_PFLOAT  => 19,
    CELL_PDOUBLE => 20,
    CELL_INT64   => 21,
    CELL_UINT64  => 22,
    CELL_PINT64  => 23,
    CELL_PUINT64 => 24,
    CELL_POINTER => 25,
    CELL_FN_SU   => 26,
    CELL_FN_TXT  => 27,
    CELL_WIDGET  => 28
  );
  for cell_type_t'size use c.unsigned'size;
  pragma convention (c, cell_type_t);

  type cell_data_text_t is array (1 .. txt_max) of aliased c.char;
  pragma convention (c, cell_data_text_t);

  type cell_data_selector_t is (sel_s, sel_i, sel_f, sel_p, sel_l, sel_u64);
  type cell_data_t (member : cell_data_selector_t := sel_s) is record
    case member is
      when sel_s   => s   : cell_data_text_t;
      when sel_i   => i   : c.int;
      when sel_f   => f   : c.double;
      when sel_p   => p   : agar.core.types.void_ptr_t;
      when sel_l   => l   : c.long;
      when sel_u64 => u64 : agar.core.types.uint64_t;
    end case;
  end record;
  pragma convention (c, cell_data_t);
  pragma unchecked_union (cell_data_t);

  type cell_format_t is array (1 .. fmt_max) of aliased c.char;
  pragma convention (c, cell_format_t);

  type cell_t is record
    cell_type : cell_type_t;
    data      : cell_data_t;
    fmt       : cell_format_t;
    fn_su     : access function
      (v : agar.core.types.void_ptr_t;
       n : c.int;
       m : c.int) return agar.gui.surface.surface_access_t;
    fn_txt    : access procedure
      (v : agar.core.types.void_ptr_t;
       s : cs.chars_ptr;
       n : c.size_t);
    widget    : widget_access_t;
    selected  : c.int;
    surface   : c.int;
  end record;
  type cell_access_t is access all cell_t;
  pragma convention (c, cell_t);
  pragma convention (c, cell_access_t);

  type column_name_t is array (1 .. col_name_max) of aliased c.char;
  pragma convention (c, column_name_t);

  subtype column_flags_t is c.unsigned;
  TABLE_COL_FILL        : constant column_flags_t := 16#01#;
  TABLE_SORT_ASCENDING  : constant column_flags_t := 16#02#;
  TABLE_SORT_DESCENDING : constant column_flags_t := 16#04#;
  TABLE_HFILL           : constant column_flags_t := 16#08#;
  TABLE_VFILL           : constant column_flags_t := 16#10#;
  TABLE_EXPAND          : constant column_flags_t := TABLE_HFILL or TABLE_VFILL;

  type column_t is record
    name     : column_name_t;
    sort_fn  : access function (a, b : agar.core.types.void_ptr_t) return c.int;
    flags    : column_flags_t;
    selected : c.int;
    w        : c.int;
    x        : c.int;
    surface  : c.int;
    pool     : cell_access_t;
    mpool    : c.unsigned;
  end record;
  type column_access_t is access all column_t;
  pragma convention (c, column_t);
  pragma convention (c, column_access_t);

  subtype table_flags_t is c.unsigned;
  TABLE_MULTI          : constant table_flags_t := 16#01#;
  TABLE_MULTITOGGLE    : constant table_flags_t := 16#02#;
  TABLE_REDRAW_CELLS   : constant table_flags_t := 16#04#;
  TABLE_POLL           : constant table_flags_t := 16#08#;
  TABLE_HIGHLIGHT_COLS : constant table_flags_t := 16#40#;
  TABLE_MULTIMODE      : constant table_flags_t := TABLE_MULTI or TABLE_MULTITOGGLE;

  type table_t is record
    widget           : widget_t;
    flags            : table_flags_t;
    selmode          : select_mode_t;
    selected_row     : agar.core.types.void_ptr_t;
    selected_col     : agar.core.types.void_ptr_t;
    selected_cell    : agar.core.types.void_ptr_t;
    pre_w            : c.int;
    pre_h            : c.int;
    sep              : cs.chars_ptr;
    w_table          : c.int;
    row_w            : c.int;
    col_h            : c.int;
    x_offset         : c.int;
    m_offset         : c.int;
    cols             : column_access_t;
    cells            : access cell_access_t;
    n                : c.unsigned;
    m                : c.unsigned;
    m_vis            : c.unsigned;
    n_resizing       : c.int;
    v_bar            : agar.gui.widget.scrollbar.scrollbar_access_t;
    h_bar            : agar.gui.widget.scrollbar.scrollbar_access_t;
    poll_ev          : agar.core.event.event_access_t;
    dbl_click_row_ev : agar.core.event.event_access_t;
    dbl_click_col_ev : agar.core.event.event_access_t;
    dbl_clicked_row  : c.int;
    dbl_clicked_col  : c.int;
    wheel_ticks      : agar.core.types.uint32_t;
    inc_to           : agar.core.timeout.timeout_t;
    dec_to           : agar.core.timeout.timeout_t;
    popups           : popup_slist.head_t;
  end record;
  type table_access_t is access all table_t;
  pragma convention (c, table_t);
  pragma convention (c, table_access_t);

end agar.gui.widget.table;
