/*	Public domain	*/

#ifndef _AGAR_GUI_FIXED_PLOTTER_H_
#define _AGAR_GUI_FIXED_PLOTTER_H_

#ifdef _AGAR_INTERNAL
#include <gui/widget.h>
#include <gui/label.h>
#else
#include <agar/gui/widget.h>
#include <agar/gui/label.h>
#endif

#include "begin_code.h"

typedef Sint16 AG_FixedPlotterValue;

struct ag_fixed_plotter;

typedef struct ag_fixed_plotter_item {
	char name[AG_LABEL_MAX];		/* Description */
	Uint32 color;				/* Line color */
	AG_FixedPlotterValue *vals;			/* Value array */
	Uint32 nvals;
	Uint32 maxvals;
	Uint32 limit;
	struct ag_fixed_plotter *fpl;			/* Back pointer */
	TAILQ_ENTRY(ag_fixed_plotter_item) items;
} AG_FixedPlotterItem;

TAILQ_HEAD(ag_fixed_plotter_itemq, ag_fixed_plotter_item);

enum ag_fixed_plotter_type {
	AG_FIXED_PLOTTER_POINTS,
	AG_FIXED_PLOTTER_LINES
};

typedef struct ag_fixed_plotter {
	struct ag_widget wid;

	enum ag_fixed_plotter_type type;
	int flags;
#define AG_FIXED_PLOTTER_SCROLL	0x01	/* Scroll if the end is not visible */
#define AG_FIXED_PLOTTER_XAXIS	0x02	/* Display X axis */
#define AG_FIXED_PLOTTER_HFILL	0x04
#define AG_FIXED_PLOTTER_VFILL	0x08
#define AG_FIXED_PLOTTER_FOCUS	0x10
#define AG_FIXED_PLOTTER_EXPAND (AG_FIXED_PLOTTER_HFILL|AG_FIXED_PLOTTER_VFILL)
	AG_FixedPlotterValue yrange;		/* Max. value */
	AG_FixedPlotterValue xoffs;		/* Display offset */
	int origin_y;			/* Origin position (%) */
	struct ag_fixed_plotter_itemq items;	/* Items to plot */
} AG_FixedPlotter;

__BEGIN_DECLS
AG_FixedPlotter *AG_FixedPlotterNew(void *, enum ag_fixed_plotter_type, Uint);
void AG_FixedPlotterInit(AG_FixedPlotter *, enum ag_fixed_plotter_type, Uint);
void AG_FixedPlotterDestroy(void *);
void AG_FixedPlotterDraw(void *);
void AG_FixedPlotterScale(void *, int, int);

AG_FixedPlotterItem *AG_FixedPlotterCurve(AG_FixedPlotter *, const char *,
		                          Uint8, Uint8, Uint8, Uint32);
void AG_FixedPlotterFreeItems(AG_FixedPlotter *);
void AG_FixedPlotterSetRange(AG_FixedPlotter *, AG_FixedPlotterValue);
void AG_FixedPlotterDatum(AG_FixedPlotterItem *, AG_FixedPlotterValue);
__inline__ void AG_FixedPlotterScroll(AG_FixedPlotter *, int);
__END_DECLS

#include "close_code.h"
#endif /* _AGAR_GUI_FIXED_PLOTTER_H_ */