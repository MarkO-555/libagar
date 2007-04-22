/*	$Csoft: tool.h,v 1.9 2005/08/01 04:09:13 vedge Exp $	*/
/*	Public domain	*/

#ifndef _AGAR_MAPEDIT_TOOL_H_
#define _AGAR_MAPEDIT_TOOL_H_
#include "begin_code.h"

#define AG_MAPTOOL_STATUS_MAX	8

struct map_view;
struct map_tool_keybinding;
struct map_tool_mousebinding;

typedef struct map_tool_ops {
	const char *name;
	const char *desc;
	int icon;

	size_t len;
	int flags;
#define TOOL_HIDDEN	0x01		/* Don't include in toolbars/menus */

	void (*init)(void *);
	void (*destroy)(void *);
	void (*edit_pane)(void *, void *);
	void (*edit)(void *);
	int (*cursor)(void *, SDL_Rect *);
	int (*effect)(void *, MAP_Node *);
	int (*mousemotion)(void *, int x, int y, int xrel, int yrel, int btn);
	int (*mousebuttondown)(void *, int x, int y, int btn);
	int (*mousebuttonup)(void *, int x, int y, int btn);
	int (*keydown)(void *, int ksym, int kmod);
	int (*keyup)(void *, int ksym, int kmod);
} MAP_ToolOps;

typedef struct map_tool {
	const MAP_ToolOps *ops;
	struct map_view *mv;			/* Associated view */
	void *p;				/* User-supplied pointer */
	char *status[AG_MAPTOOL_STATUS_MAX];	/* Status message stack */
	int nstatus;
	AG_Window *win;		/* Edition window (if any) */
	AG_Widget *pane;		/* Edition pane (if any) */
	AG_Button *trigger;		/* Trigger button (XXX) */

	SLIST_HEAD(,map_tool_keybinding) kbindings;
	SLIST_HEAD(,map_tool_mousebinding) mbindings;
	TAILQ_ENTRY(map_tool) tools;
} MAP_Tool;

typedef struct map_tool_keybinding {
	SDLMod mod;
	SDLKey key;
	int edit;
	int (*func)(MAP_Tool *, SDLKey k, int s, void *);
	void *arg;
	SLIST_ENTRY(map_tool_keybinding) kbindings;
} MAP_ToolKeyBinding;

typedef struct map_tool_mousebinding {
	int button;
	int edit;
	int (*func)(MAP_Tool *, int b, int s, int x, int y, void *);
	void *arg;
	SLIST_ENTRY(map_tool_mousebinding) mbindings;
} MAP_ToolMouseBinding;

#define TOOL(t) ((MAP_Tool *)(t))

__BEGIN_DECLS
void		 MAP_ToolInit(MAP_Tool *);
void		 MAP_ToolDestroy(MAP_Tool *);
AG_Window	*MAP_ToolWindow(void *, const char *);

void MAP_ToolBindKey(void *, SDLMod, SDLKey,
		   int (*)(MAP_Tool *, SDLKey, int, void *), void *);
void MAP_ToolBindMouseButton(void *, int,
			   int (*)(MAP_Tool *, int, int, int, int, void *),
			   void *);
void MAP_ToolUnbindKey(void *, SDLMod, SDLKey);

void MAP_ToolPushStatus(void *, const char *, ...);
void MAP_ToolSetStatus(void *, const char *, ...);
void MAP_ToolPopStatus(void *);
void MAP_ToolUpdateStatus(void *);
__END_DECLS

#include "close_code.h"
#endif /* _AGAR_MAPEDIT_TOOL_H_ */