import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {

  title = 'frontend';
  navbarItems = [{
    label: 'foo',
    link: 'bar',
    isActive: true,
    cls: ''
  }]

}
