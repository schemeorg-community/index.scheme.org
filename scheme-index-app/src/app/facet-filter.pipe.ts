import { Pipe, PipeTransform } from '@angular/core';
import { FacetOption } from './filter-pane/filter-pane.component';

@Pipe({
  name: 'facetFilter',
  standalone: true
})
export class FacetFilterPipe implements PipeTransform {

  transform(value: FacetOption[], filter: string): FacetOption[] {
      if (!filter)
          return value;
      return value.filter(f => f.label.indexOf(filter) != -1);
  }

}
