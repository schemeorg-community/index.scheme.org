import { ErrorHandler, NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { BrowserModule } from '@angular/platform-browser';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { IndexPageComponent } from './index-page/index-page.component';
import { FilterPaneComponent } from './filter-pane/filter-pane.component';
import { SearchPageComponent } from './search-page/search-page.component';
import { HttpClientModule } from '@angular/common/http';
import { SearchItemComponent } from './search-item/search-item.component';
import { SingleEntryPageComponent } from './single-entry-page/single-entry-page.component';
import { PagerComponent } from './pager/pager.component';
import { IndexErrorHandler } from './index-error-handler';

import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { FacetFilterPipe } from './facet-filter.pipe';

@NgModule({
    declarations: [
        AppComponent,
        IndexPageComponent,
        FilterPaneComponent,
        SearchPageComponent,
        SearchItemComponent,
        SingleEntryPageComponent,
        PagerComponent,
        FacetFilterPipe
    ],
    imports: [
        FormsModule,
        BrowserModule,
        HttpClientModule,
        AppRoutingModule,
        FontAwesomeModule
    ],
    providers: [
        IndexErrorHandler, {
        provide: ErrorHandler, useExisting: IndexErrorHandler
    }],
    bootstrap: [AppComponent]
})
export class AppModule { }
