import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'facetFilter'
})
export class FacetFilterPipe implements PipeTransform {

  transform(value: any[], filter: string): any[] {
      if (!filter)
          return value;
      return value.filter(f => f.label.indexOf(filter) != -1);
  }

}
