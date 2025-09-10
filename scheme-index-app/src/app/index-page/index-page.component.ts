import { Component, inject } from '@angular/core';
import { Observable, map } from 'rxjs';
import { IndexService } from '../index.service';
import { RouterModule } from '@angular/router';
import { CommonModule } from '@angular/common';
import { LoaderComponent } from '../loader/loader.component';

@Component({
    imports: [
        CommonModule,
        RouterModule,
        LoaderComponent
    ],
    selector: 'app-index-page',
    templateUrl: './index-page.component.html',
    styleUrls: ['./index-page.component.scss']
})
export class IndexPageComponent {
    filtersetsService = inject(IndexService);


    filtersetGroups: Observable<FiltersetGroup[]>;

    constructor() {
        const filtersetsService = this.filtersetsService;

        const isRnrs = (code: string) => {
            return code.match(/r.rs/);
        };
        this.filtersetGroups = filtersetsService.filtersets$.pipe(map(filtersets => {
            return [{
                filtersets: filtersets.filter(f => isRnrs(f.code))
            }, {
                filtersets: filtersets.filter(f => !isRnrs(f.code))
            }].filter(e => e.filtersets.length);
        }));
    }
}

interface FiltersetGroup {
    filtersets: Filterset[];
}

interface Filterset {
    code: string;
    name: string;
}
