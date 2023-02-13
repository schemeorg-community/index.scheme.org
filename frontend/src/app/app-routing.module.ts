import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { IndexPageComponent } from './index-page/index-page.component';
import { SearchPageComponent } from './search-page/search-page.component';

const routes: Routes = [{
  path: '', component: IndexPageComponent
}, {
  path: 'filterset/:filterset/search', component: SearchPageComponent
}];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
