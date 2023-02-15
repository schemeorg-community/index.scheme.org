import { Component } from '@angular/core';
import { Observable, map } from 'rxjs';
import { FiltersetsService } from '../filtersets-service.service';

@Component({
  selector: 'app-index-page',
  templateUrl: './index-page.component.html',
  styleUrls: ['./index-page.component.scss']
})
export class IndexPageComponent {

  filtersetGroups: Observable<FiltersetGroup[]>;

  constructor(filtersetsService: FiltersetsService) {
    this.filtersetGroups = filtersetsService.filtersets.pipe(map(codes => {
      return [{
        filtersets: codes.map(code => {
          return {
            code,
            name: code
          };
        })
      }];
    }));
  }

  downloads: Download[] = [];

  exampleUrl = "curl 'https://index.scheme.org/rest/filterset/chibi/search?query=define-record-type&facet=false'";

  exampleResponse = `{"items":[{"lib":"(scheme
            base)","name":"define-record-type","type":"syntax","func_signature":null,"syntax_signature":{"literals":[],"patterns":[{"pattern":"(name
            constructor pred field
            ...)","type":null}]},"func_param_signatures":[],"syntax_subsyntax_signatures":[{"name":"constructor","patterns":["(constructor-name
            field-name ...)"]},{"name":"field","patterns":["(field-name accessor-name)","(field-name accessor-name
            modifier-name)"]}],"syntax_param_signatures":[],"tags":[],"param_types":[],"return_types":[],"parameterized_by":[],"spec_values":[],"super_types":[]}],"total":1}`;

}


interface Download {
  url: string;
  checksum: string;
  name: string;
}

interface FiltersetGroup {
  filtersets: Filterset[];
}

interface Filterset {
  code: string;
  name: string;
}
