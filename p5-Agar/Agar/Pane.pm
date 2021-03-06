package Agar::Pane;

use strict;
use Agar;

1;

__END__

=head1 NAME

Agar::Pane - 2 Box widgets arranged as panes

=head1 SYNOPSIS

  use Agar;
  use Agar::Pane;
  
  Agar::Pane->newVert($parent);

=head1 DESCRIPTION

Please see AG_Pane(3) for a
full explanation of what its methods do and what bindings and events
it defines, if any.

=head1 INHERITANCE HIERARCHY

L<Agar::Object(3)> -> L<Agar::Widget(3)> -> B<Agar::Pane>

=head1 METHODS

=over 4

=item B<$widget = Agar::Pane-E<gt>newHoriz($parent,[%options])>

=item B<$widget = Agar::Pane-E<gt>newVert($parent,[%options]>

Constructors.

Recognised options include:

=over 4

=item C<div1Fill>

=item C<forceDiv1Fill>

=item C<frame>

=item C<div>

=item C<forceDiv>

=item C<unmovable>

Z<>

=back

=item B<$widget-E<gt>setDividerWidth($pixels)>

=item B<$widget-E<gt>setDivisionMin($pixels)>

=item B<$box_widget = $widget-E<gt>leftPane()>

=item B<$box_widget = $widget-E<gt>rightPane()>

=item B<$box_widget = $widget-E<gt>topPane()>

=item B<$box_widget = $widget-E<gt>bottomPane()>

=item B<$widget-E<gt>moveDivider($x)>

=item B<$widget-E<gt>moveDividerPct($percent)>

=back

=head1 AUTHOR

Julien Nadeau E<lt>F<vedge@hypertriton.com>E<gt>

Mat Sutcliffe E<lt>F<oktal@gmx.co.uk>E<gt>

=head1 COPYRIGHT

Copyright (c) 2009-2016 Hypertriton, Inc. All rights reserved.
This program is free software. You can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 SEE ALSO

L<Agar(3)>, L<Agar::Box(3)>, L<Agar::Fixed(3)>, L<Agar::MPane(3)>,
L<Agar::Scrollview(3)>, L<Agar::Window(3)>

=cut
